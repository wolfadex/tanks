module Main exposing (..)

import Angle exposing (Angle)
import Axis3d
import Block3d
import Browser exposing (Document)
import Browser.Events
import Camera3d exposing (Camera3d)
import Color
import Cylinder3d
import Direction3d
import Frame3d
import Html exposing (Html)
import Json.Decode exposing (Value)
import Length exposing (Meters)
import Pixels
import Point3d exposing (Point3d)
import Quantity exposing (Quantity)
import Scene3d
import Scene3d.Material exposing (Material)
import Sphere3d
import Vector3d
import Viewpoint3d


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { tank : Tank }


type alias Tank =
    { forward : Angle
    , position : Point3d Meters World
    , cannonRotation : Angle
    , cannonPitch : Angle
    }


type World
    = World Never


init : () -> ( Model, Cmd Msg )
init () =
    ( { tank =
            { forward = Angle.degrees 0
            , position = Point3d.origin
            , cannonRotation = Angle.degrees 0
            , cannonPitch = Angle.degrees 0
            }
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onKeyPress (Json.Decode.map KeyPress Json.Decode.value)


type Msg
    = NoOp
    | KeyPress Value


type GameAction
    = MoveForward
    | MoveBackward
    | RotateClockwise
    | RotateCounterClockwise


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        KeyPress value ->
            case Json.Decode.decodeValue decodeMove value |> Debug.log "decode" of
                Err _ ->
                    ( model, Cmd.none )

                Ok MoveForward ->
                    ( { model | tank = moveTank 1.0 model.tank }
                    , Cmd.none
                    )

                Ok MoveBackward ->
                    ( { model | tank = moveTank -1.0 model.tank }
                    , Cmd.none
                    )

                Ok RotateClockwise ->
                    ( { model | tank = rotateTank 1.0 model.tank }
                    , Cmd.none
                    )

                Ok RotateCounterClockwise ->
                    ( { model | tank = rotateTank -1.0 model.tank }
                    , Cmd.none
                    )


moveTank : Float -> Tank -> Tank
moveTank magnitude tank =
    { tank
        | position =
            Point3d.translateBy
                (Vector3d.meters magnitude 0.0 0.0
                    |> Vector3d.rotateAround Axis3d.z tank.forward
                )
                tank.position
    }


rotateTank : Float -> Tank -> Tank
rotateTank magnitude tank =
    { tank
        | forward =
            Quantity.plus tank.forward (Angle.degrees (magnitude * 45.0))
    }


decodeMove : Json.Decode.Decoder GameAction
decodeMove =
    Json.Decode.andThen
        (\key ->
            case Debug.log "key" key of
                "w" ->
                    Json.Decode.succeed MoveForward

                "s" ->
                    Json.Decode.succeed MoveBackward

                "d" ->
                    Json.Decode.succeed RotateCounterClockwise

                "a" ->
                    Json.Decode.succeed RotateClockwise

                _ ->
                    Json.Decode.fail "Other key"
        )
        (Json.Decode.field "key" Json.Decode.string)


view : Model -> Document Msg
view model =
    { title = "Tanks"
    , body = [ game3dScene model ]
    }


game3dScene : Model -> Html Msg
game3dScene model =
    let
        -- Define a camera as usual
        camera : Camera3d Meters coordinates
        camera =
            Camera3d.perspective
                { viewpoint =
                    Viewpoint3d.lookAt
                        { focalPoint = Point3d.origin
                        , eyePoint = Point3d.meters 30 20 30
                        , upDirection = Direction3d.positiveZ
                        }
                , verticalFieldOfView = Angle.degrees 30
                }

        ground : Scene3d.Entity coordinates
        ground =
            Scene3d.quad
                (Scene3d.Material.nonmetal
                    { baseColor = Color.green
                    , roughness = 0.8
                    }
                )
                (Point3d.fromMeters { x = -10, y = -10, z = 0 })
                (Point3d.fromMeters { x = -10, y = 10, z = 0 })
                (Point3d.fromMeters { x = 10, y = 10, z = 0 })
                (Point3d.fromMeters { x = 10, y = -10, z = 0 })
    in
    Scene3d.sunny
        { camera = camera
        , clipDepth = Length.centimeters 0.5
        , dimensions = ( Pixels.int 1200, Pixels.int 720 )
        , background = Scene3d.backgroundColor Color.black
        , entities =
            [ ground
            , viewTank model.tank
            ]
        , shadows = True

        -- Specify the global up direction (this controls the orientation of
        -- the sky light)
        , upDirection = Direction3d.positiveZ

        -- Specify the direction of incoming sunlight (note that this is the
        -- opposite of the direction *to* the sun)
        , sunlightDirection = Direction3d.yz (Angle.degrees -120)
        }


viewTank : Tank -> Scene3d.Entity World
viewTank tank =
    let
        material =
            Scene3d.Material.metal { baseColor = Color.lightBlue, roughness = 0.5 }

        generalPosition : Frame3d.Frame3d Meters World defines2
        generalPosition =
            Frame3d.atPoint Point3d.origin
                |> Frame3d.rotateAroundOwn (\_ -> Axis3d.z) tank.forward
                |> Frame3d.translateBy (Vector3d.from Point3d.origin tank.position)

        -- (Point3d.translateIn Direction3d.positiveZ (Length.meters 0.5) tank.position)
        topPosition : Frame3d.Frame3d Meters World defines2
        topPosition =
            Frame3d.translateIn Direction3d.positiveZ (Length.meters 0.5) generalPosition

        cannonPosition =
            generalPosition
                |> Frame3d.translateIn Direction3d.positiveZ (Length.meters 0.5)
                |> Frame3d.translateIn (Direction3d.xy tank.forward) (Length.meters 0.5)
    in
    Scene3d.group
        [ Scene3d.blockWithShadow
            material
            (Block3d.centeredOn
                generalPosition
                ( Length.meters 2.0
                , Length.meters 1.0
                , Length.meters 0.5
                )
            )
        , Scene3d.cylinderWithShadow
            material
            (Cylinder3d.startingAt
                Point3d.origin
                Direction3d.positiveZ
                { radius = Length.meters 0.5
                , length = Length.meters 0.25
                }
                |> Cylinder3d.placeIn topPosition
            )
        , Scene3d.cylinderWithShadow
            material
            (Cylinder3d.startingAt
                Point3d.origin
                Direction3d.positiveX
                { radius = Length.meters 0.125
                , length = Length.meters 1.0
                }
                |> Cylinder3d.placeIn cannonPosition
            )
        ]
