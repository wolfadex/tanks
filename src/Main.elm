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
import Quantity
import Scene3d
import Scene3d.Material exposing (Material)
import Speed
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
    , rotation : Angle
    }


type World
    = World Never


init : () -> ( Model, Cmd Msg )
init () =
    ( { tank =
            { forward = Angle.degrees 0
            , position = Point3d.origin
            , rotation = Angle.degrees 0
            }
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onKeyPress (Json.Decode.map KeyPress Json.Decode.value)


type Msg
    = NoOp
      -- | Move DirectionGame
    | KeyPress Value


type DirectionGame
    = Forward
    | Reverse


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        KeyPress value ->
            case Json.Decode.decodeValue decodeMove value |> Debug.log "decode" of
                Err err ->
                    ( model, Cmd.none )

                Ok direction ->
                    let
                        tank =
                            model.tank
                    in
                    ( { model
                        | tank =
                            { tank
                                | position =
                                    let
                                        frame =
                                            Frame3d.atPoint tank.position
                                                |> Frame3d.rotateAround Axis3d.z tank.forward

                                        dir =
                                            Vector3d.xyzIn frame
                                                (Length.meters
                                                    (case direction of
                                                        Forward ->
                                                            1.0

                                                        Reverse ->
                                                            -1.0
                                                    )
                                                )
                                                (Length.meters 0.0)
                                                (Length.meters 0.0)
                                    in
                                    Point3d.translateBy dir tank.position
                            }
                      }
                    , Cmd.none
                    )


decodeMove : Json.Decode.Decoder DirectionGame
decodeMove =
    Json.Decode.andThen
        (\key ->
            case Debug.log "key" key of
                "w" ->
                    Json.Decode.succeed Forward

                "s" ->
                    Json.Decode.succeed Reverse

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
        -- Define a blue nonmetal (plastic or similar) material
        material : Material coordinates { a | normals : () }
        material =
            Scene3d.Material.nonmetal
                { baseColor = Color.lightBlue
                , roughness = 0.4 -- varies from 0 (mirror-like) to 1 (matte)
                }

        -- Create a sphere entity using the defined material
        sphereEntity : Scene3d.Entity coordinates
        sphereEntity =
            Scene3d.sphere material <|
                Sphere3d.withRadius (Length.meters 5) Point3d.origin

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
    -- Use the preset 'Scene3d.sunny' which handles most of the lighting details
    -- for us (creates one direct light source approximating sunlight, and
    -- another soft light source representing sky light and light reflected from
    -- surrounding objects)
    Scene3d.sunny
        { camera = camera
        , clipDepth = Length.centimeters 0.5
        , dimensions = ( Pixels.int 800, Pixels.int 600 )
        , background = Scene3d.backgroundColor Color.black
        , entities =
            [ ground

            -- , sphereEntity
            , viewTank model.tank
            ]

        -- Specify that sunlight should not cast shadows (since we wouldn't see
        -- them anyways in this scene)
        , shadows = True

        -- Specify the global up direction (this controls the orientation of
        -- the sky light)
        , upDirection = Direction3d.positiveZ

        -- Specify the direction of incoming sunlight (note that this is the
        -- opposite of the direction *to* the sun)
        , sunlightDirection = Direction3d.yz (Angle.degrees -120)
        }


viewTank tank =
    let
        material =
            Scene3d.Material.metal { baseColor = Color.blue, roughness = 0.5 }
    in
    Scene3d.group
        [ Scene3d.blockWithShadow
            material
            (Block3d.centeredOn
                (Frame3d.atPoint
                    (Point3d.translateIn Direction3d.positiveZ (Length.meters 0.5) tank.position)
                )
                ( Length.meters 2.0
                , Length.meters 1.0
                , Length.meters 0.5
                )
            )
        , Scene3d.cylinderWithShadow
            material
            (Cylinder3d.centeredOn
                (Point3d.translateIn Direction3d.positiveZ (Length.meters 1) tank.position)
                Direction3d.positiveZ
                { radius = Length.meters 0.5
                , length = Length.meters 0.25
                }
            )
        ]
