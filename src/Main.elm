module Main exposing (main)

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
import Scene3d.Material
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
    { tickTime : Float
    , tank : Tank
    , forwardKey : KeyPressesd
    , backwardKey : KeyPressesd
    , rotateClockwiseKey : KeyPressesd
    , rotateCounterClockwiseKey : KeyPressesd
    , rotateCannonClockwiseKey : KeyPressesd
    , rotateCannonCounterClockwiseKey : KeyPressesd
    , aimCannonUp : KeyPressesd
    , aimCannonDown : KeyPressesd
    }


type KeyPressesd
    = Pressed
    | Unpressed


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
    ( { tickTime = 0
      , tank =
            { forward = Angle.degrees 0
            , position = Point3d.origin
            , cannonRotation = Angle.degrees 0
            , cannonPitch = Angle.degrees 0
            }
      , forwardKey = Unpressed
      , backwardKey = Unpressed
      , rotateClockwiseKey = Unpressed
      , rotateCounterClockwiseKey = Unpressed
      , rotateCannonClockwiseKey = Unpressed
      , rotateCannonCounterClockwiseKey = Unpressed
      , aimCannonUp = Unpressed
      , aimCannonDown = Unpressed
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onKeyDown (Json.Decode.map KeyDown Json.Decode.value)
        , Browser.Events.onKeyUp (Json.Decode.map KeyUp Json.Decode.value)
        , Browser.Events.onAnimationFrameDelta Tick
        ]


type Msg
    = Tick Float
    | KeyDown Value
    | KeyUp Value


type GameAction
    = MoveForward
    | MoveBackward
    | RotateClockwise
    | RotateCounterClockwise
    | CannonRotateClockwise
    | CannonRotateCounterClockwise
    | AimUp
    | AimDown


framerate : Float
framerate =
    16.6


applyTick : Model -> Model
applyTick model =
    if model.tickTime >= framerate then
        let
            remainingTickTime =
                model.tickTime - framerate

            nextModel =
                { model
                    | tickTime = remainingTickTime
                    , tank =
                        model.tank
                            |> moveTank
                                (case ( model.forwardKey, model.backwardKey ) of
                                    ( Pressed, Unpressed ) ->
                                        1.0

                                    ( Unpressed, Pressed ) ->
                                        -1.0

                                    _ ->
                                        0.0
                                )
                            |> rotateTank
                                (case ( model.rotateClockwiseKey, model.rotateCounterClockwiseKey ) of
                                    ( Pressed, Unpressed ) ->
                                        1.0

                                    ( Unpressed, Pressed ) ->
                                        -1.0

                                    _ ->
                                        0.0
                                )
                            |> rotateCannon
                                (case ( model.rotateCannonClockwiseKey, model.rotateCannonCounterClockwiseKey ) of
                                    ( Pressed, Unpressed ) ->
                                        1.0

                                    ( Unpressed, Pressed ) ->
                                        -1.0

                                    _ ->
                                        0.0
                                )
                            |> aimCannon
                                (case ( model.aimCannonUp, model.aimCannonDown ) of
                                    ( Pressed, Unpressed ) ->
                                        1.0

                                    ( Unpressed, Pressed ) ->
                                        -1.0

                                    _ ->
                                        0.0
                                )
                }
        in
        if remainingTickTime < framerate then
            nextModel

        else
            applyTick nextModel

    else
        model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick deltaMs ->
            ( applyTick { model | tickTime = model.tickTime + deltaMs }
            , Cmd.none
            )

        KeyDown value ->
            case Json.Decode.decodeValue decodeKeyToAction value of
                Err _ ->
                    ( model, Cmd.none )

                Ok MoveForward ->
                    ( { model | forwardKey = Pressed }, Cmd.none )

                Ok MoveBackward ->
                    ( { model | backwardKey = Pressed }, Cmd.none )

                Ok RotateClockwise ->
                    ( { model | rotateClockwiseKey = Pressed }, Cmd.none )

                Ok RotateCounterClockwise ->
                    ( { model | rotateCounterClockwiseKey = Pressed }, Cmd.none )

                Ok CannonRotateClockwise ->
                    ( { model | rotateCannonClockwiseKey = Pressed }, Cmd.none )

                Ok CannonRotateCounterClockwise ->
                    ( { model | rotateCannonCounterClockwiseKey = Pressed }, Cmd.none )

                Ok AimUp ->
                    ( { model | aimCannonUp = Pressed }, Cmd.none )

                Ok AimDown ->
                    ( { model | aimCannonDown = Pressed }, Cmd.none )

        KeyUp value ->
            case Json.Decode.decodeValue decodeKeyToAction value of
                Err _ ->
                    ( model, Cmd.none )

                Ok MoveForward ->
                    ( { model | forwardKey = Unpressed }, Cmd.none )

                Ok MoveBackward ->
                    ( { model | backwardKey = Unpressed }, Cmd.none )

                Ok RotateClockwise ->
                    ( { model | rotateClockwiseKey = Unpressed }, Cmd.none )

                Ok RotateCounterClockwise ->
                    ( { model | rotateCounterClockwiseKey = Unpressed }, Cmd.none )

                Ok CannonRotateClockwise ->
                    ( { model | rotateCannonClockwiseKey = Unpressed }, Cmd.none )

                Ok CannonRotateCounterClockwise ->
                    ( { model | rotateCannonCounterClockwiseKey = Unpressed }, Cmd.none )

                Ok AimUp ->
                    ( { model | aimCannonUp = Unpressed }, Cmd.none )

                Ok AimDown ->
                    ( { model | aimCannonDown = Unpressed }, Cmd.none )


moveTank : Float -> Tank -> Tank
moveTank magnitude tank =
    { tank
        | position =
            Point3d.translateBy
                (Vector3d.meters (magnitude * 0.05) 0.0 0.0
                    |> Vector3d.rotateAround Axis3d.z tank.forward
                )
                tank.position
    }


rotateTank : Float -> Tank -> Tank
rotateTank magnitude tank =
    { tank
        | forward =
            Quantity.plus tank.forward (Angle.degrees (magnitude * 1.0))
    }


rotateCannon : Float -> Tank -> Tank
rotateCannon magnitude tank =
    { tank
        | cannonRotation =
            Quantity.plus tank.cannonRotation (Angle.degrees (magnitude * 1.0))
    }


aimCannon : Float -> Tank -> Tank
aimCannon magnitude tank =
    { tank
        | cannonPitch =
            tank.cannonPitch
                |> Quantity.plus (Angle.degrees (magnitude * -1.0))
                |> Quantity.max (Angle.degrees -45)
                |> Quantity.min (Angle.degrees 0)
    }


decodeKeyToAction : Json.Decode.Decoder GameAction
decodeKeyToAction =
    Json.Decode.andThen
        (\key ->
            case key of
                "w" ->
                    Json.Decode.succeed MoveForward

                "s" ->
                    Json.Decode.succeed MoveBackward

                "d" ->
                    Json.Decode.succeed RotateCounterClockwise

                "a" ->
                    Json.Decode.succeed RotateClockwise

                "e" ->
                    Json.Decode.succeed CannonRotateCounterClockwise

                "q" ->
                    Json.Decode.succeed CannonRotateClockwise

                "r" ->
                    Json.Decode.succeed AimUp

                "f" ->
                    Json.Decode.succeed AimDown

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

        bodyPosition : Frame3d.Frame3d Meters World defines2
        bodyPosition =
            Frame3d.atPoint Point3d.origin
                |> Frame3d.rotateAroundOwn (\_ -> Axis3d.z) tank.forward
                |> Frame3d.translateBy (Vector3d.from Point3d.origin tank.position)
                |> Frame3d.translateIn Direction3d.positiveZ (Length.meters 0.25)

        topPosition : Frame3d.Frame3d Meters World defines2
        topPosition =
            Frame3d.atPoint Point3d.origin
                |> Frame3d.rotateAroundOwn (\_ -> Axis3d.z) (Quantity.plus tank.forward tank.cannonRotation)
                |> Frame3d.translateBy (Vector3d.from Point3d.origin tank.position)
                |> Frame3d.translateIn Direction3d.positiveZ (Length.meters 0.5)

        cannonPosition : Frame3d.Frame3d Meters World defines2
        cannonPosition =
            Frame3d.atPoint Point3d.origin
                |> Frame3d.translateIn Direction3d.positiveX (Length.meters 0.5)
                |> Frame3d.rotateAroundOwn (\_ -> Axis3d.y) tank.cannonPitch
                |> Frame3d.rotateAroundOwn (\_ -> Axis3d.z) (Quantity.plus tank.forward tank.cannonRotation)
                |> Frame3d.translateBy (Vector3d.from Point3d.origin tank.position)
                |> Frame3d.translateIn Direction3d.positiveZ (Length.meters 0.5)
    in
    Scene3d.group
        [ Scene3d.blockWithShadow
            material
            (Block3d.centeredOn bodyPosition
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
