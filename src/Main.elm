module Main exposing (main)

import Acceleration
import Angle exposing (Angle)
import Axis3d
import Block3d exposing (Block3d)
import Browser exposing (Document)
import Browser.Events
import Camera3d exposing (Camera3d)
import Color
import Cylinder3d
import Direction3d
import Duration
import Force
import Frame3d
import Html exposing (Html)
import Json.Decode exposing (Value)
import Length exposing (Meters)
import LineSegment3d
import Mass
import Physics.Body exposing (Body)
import Physics.Contact
import Physics.Coordinates exposing (BodyCoordinates, WorldCoordinates)
import Physics.World exposing (World)
import Pixels
import Point3d exposing (Point3d)
import Quantity
import Scene3d
import Scene3d.Material
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
    { elapsedTime : Float
    , nextId : Int
    , tank : Tank
    , playerId : Int
    , forwardKey : KeyPressesd
    , backwardKey : KeyPressesd
    , rotateClockwiseKey : KeyPressesd
    , rotateCounterClockwiseKey : KeyPressesd
    , rotateCannonClockwiseKey : KeyPressesd
    , rotateCannonCounterClockwiseKey : KeyPressesd
    , aimCannonUp : KeyPressesd
    , aimCannonDown : KeyPressesd
    , fireCannon : KeyPressesd
    , lastCannonFiredAt : Float
    , physicsWorld : World Entity
    }


type Entity
    = Ground Int
    | CannonBall Int
    | WallPermanent Int (Block3d Meters WorldCoordinates)
    | ETank Int Tank


type KeyPressesd
    = Pressed
    | Unpressed


type alias Tank =
    { cannonRotation : Angle
    , cannonPitch : Angle
    , material : Scene3d.Material.Uniform WorldCoordinates
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( { elapsedTime = 0
      , nextId = 6
      , tank =
            { cannonRotation = Angle.degrees 0
            , cannonPitch = Angle.degrees 0
            , material = Scene3d.Material.metal { baseColor = Color.lightBlue, roughness = 0.5 }
            }
      , playerId = 5
      , forwardKey = Unpressed
      , backwardKey = Unpressed
      , rotateClockwiseKey = Unpressed
      , rotateCounterClockwiseKey = Unpressed
      , rotateCannonClockwiseKey = Unpressed
      , rotateCannonCounterClockwiseKey = Unpressed
      , aimCannonUp = Unpressed
      , aimCannonDown = Unpressed
      , fireCannon = Unpressed
      , lastCannonFiredAt = 0
      , physicsWorld =
            Physics.World.empty
                |> Physics.World.withGravity (Acceleration.metersPerSecondSquared 9.80665) Direction3d.negativeZ
                |> Physics.World.add (Physics.Body.plane (Ground 0))
                |> Physics.World.add
                    (let
                        block =
                            Block3d.from (Point3d.meters 10 4 0) (Point3d.meters 11 -4 2)
                     in
                     Physics.Body.block block (WallPermanent 1 block)
                    )
                |> Physics.World.add
                    (let
                        block =
                            Block3d.from (Point3d.meters 4 -10 0) (Point3d.meters -4 -11 2)
                     in
                     Physics.Body.block block (WallPermanent 2 block)
                    )
                |> Physics.World.add
                    (let
                        block =
                            Block3d.from (Point3d.meters -10 -4 0) (Point3d.meters -11 4 2)
                     in
                     Physics.Body.block block (WallPermanent 3 block)
                    )
                |> Physics.World.add
                    (let
                        block =
                            Block3d.from (Point3d.meters -4 10 0) (Point3d.meters 4 11 2)
                     in
                     Physics.Body.block block (WallPermanent 4 block)
                    )
                |> Physics.World.add
                    (Physics.Body.block
                        (Block3d.centeredOn Frame3d.atOrigin
                            ( Length.meters 2.0
                            , Length.meters 1.0
                            , Length.meters 0.5
                            )
                        )
                        (ETank 5
                            { cannonRotation = Angle.degrees 0
                            , cannonPitch = Angle.degrees 0
                            , material = Scene3d.Material.metal { baseColor = Color.lightBlue, roughness = 0.5 }
                            }
                        )
                        |> Physics.Body.withBehavior (Physics.Body.dynamic (Mass.kilograms 1000))
                    )
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
    | FireCannon


applyTick : Float -> Model -> Model
applyTick deltaMs model =
    let
        ( cannonBallMaybeAdded, newLastCannonBallFireTime, nextId ) =
            case model.fireCannon of
                Pressed ->
                    if model.elapsedTime - model.lastCannonFiredAt > 100 then
                        case
                            Physics.World.bodies model.physicsWorld
                                |> findInList
                                    (\body ->
                                        let
                                            data =
                                                Physics.Body.data body
                                        in
                                        case data of
                                            ETank id tankData ->
                                                if id == model.playerId then
                                                    Just ( body, tankData )

                                                else
                                                    Nothing

                                            _ ->
                                                Nothing
                                    )
                        of
                            Nothing ->
                                ( model.physicsWorld, model.lastCannonFiredAt, model.nextId )

                            Just ( playerTankBody, playerTankData ) ->
                                ( Physics.World.add
                                    (let
                                        position : Frame3d.Frame3d Meters WorldCoordinates { defines : Physics.Coordinates.BodyCoordinates }
                                        position =
                                            Physics.Body.frame playerTankBody
                                                |> Frame3d.translateAlongOwn Frame3d.zAxis (Length.meters 0.5)
                                                |> Frame3d.rotateAroundOwn Frame3d.zAxis playerTankData.cannonRotation
                                                |> Frame3d.rotateAroundOwn Frame3d.yAxis playerTankData.cannonPitch
                                                |> Frame3d.translateAlongOwn Frame3d.xAxis (Length.meters 1.5)

                                        cannonBallBody : Body Entity
                                        cannonBallBody =
                                            Physics.Body.sphere
                                                (Sphere3d.atPoint Point3d.origin (Length.meters 0.25))
                                                (CannonBall model.nextId)
                                                |> Physics.Body.moveTo (Frame3d.originPoint position)
                                                |> Physics.Body.withBehavior (Physics.Body.dynamic (Mass.kilograms 5.5))
                                     in
                                     cannonBallBody
                                        |> Physics.Body.applyImpulse
                                            (Force.newtons 150 |> Quantity.times (Duration.seconds 0.5))
                                            (Frame3d.xDirection position)
                                            (Physics.Body.originPoint cannonBallBody)
                                    )
                                    model.physicsWorld
                                , model.elapsedTime
                                , model.nextId + 1
                                )

                    else
                        ( model.physicsWorld, model.lastCannonFiredAt, model.nextId )

                Unpressed ->
                    ( model.physicsWorld, model.lastCannonFiredAt, model.nextId )

        simulatedWorld =
            cannonBallMaybeAdded
                |> Physics.World.simulate (Duration.milliseconds deltaMs)
                |> Physics.World.update
                    (\body ->
                        case Physics.Body.data body of
                            ETank id tank ->
                                Physics.Body.withData
                                    (ETank id
                                        (tank
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
                                        )
                                    )
                                    body
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

                            _ ->
                                body
                    )

        contacts =
            Physics.World.contacts simulatedWorld
    in
    { model
        | elapsedTime = model.elapsedTime + deltaMs
        , nextId = nextId
        , lastCannonFiredAt = newLastCannonBallFireTime
        , physicsWorld =
            simulatedWorld
                |> Physics.World.keepIf
                    (\body ->
                        case Physics.Body.data body of
                            Ground _ ->
                                True

                            WallPermanent _ _ ->
                                True

                            ETank _ _ ->
                                True

                            _ ->
                                not (isInContact body contacts)
                    )
    }


findInList : (a -> Maybe b) -> List a -> Maybe b
findInList predicate list =
    case list of
        [] ->
            Nothing

        next :: rest ->
            case predicate next of
                Just b ->
                    Just b

                Nothing ->
                    findInList predicate rest


isInContact : Body Entity -> List (Physics.Contact.Contact Entity) -> Bool
isInContact body contacts =
    case contacts of
        [] ->
            False

        next :: rest ->
            let
                ( leftBody, rightBody ) =
                    Physics.Contact.bodies next
            in
            body == leftBody || body == rightBody || isInContact body rest


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick deltaMs ->
            ( applyTick deltaMs model
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

                Ok FireCannon ->
                    ( { model | fireCannon = Pressed }, Cmd.none )

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

                Ok FireCannon ->
                    ( { model | fireCannon = Unpressed }, Cmd.none )


moveTank : Float -> Body Entity -> Body Entity
moveTank magnitude body =
    Physics.Body.translateBy
        (Vector3d.placeIn
            (Physics.Body.frame body)
            (Vector3d.meters (magnitude * 0.05) 0.0 0.0)
        )
        body


rotateTank : Float -> Body Entity -> Body Entity
rotateTank magnitude body =
    Physics.Body.rotateAround
        (Frame3d.zAxis (Physics.Body.frame body))
        (Angle.degrees (magnitude * 1.0))
        body


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

                " " ->
                    Json.Decode.succeed FireCannon

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
                        , eyePoint = Point3d.meters 0 20 20
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
                (Point3d.fromMeters { x = -100, y = -100, z = 0 })
                (Point3d.fromMeters { x = -100, y = 100, z = 0 })
                (Point3d.fromMeters { x = 100, y = 100, z = 0 })
                (Point3d.fromMeters { x = 100, y = -100, z = 0 })
    in
    Scene3d.sunny
        { camera = camera
        , clipDepth = Length.centimeters 0.5
        , dimensions = ( Pixels.int 1200, Pixels.int 720 )
        , background = Scene3d.backgroundColor Color.black
        , entities =
            ground
                -- Debugging
                -- :: Scene3d.lineSegment (Scene3d.Material.color Color.red)
                --     (LineSegment3d.along Axis3d.z (Length.meters 0) (Length.meters 10))
                -- :: Scene3d.lineSegment (Scene3d.Material.color Color.blue)
                --     (LineSegment3d.along Axis3d.x (Length.meters 0) (Length.meters 10))
                -- :: Scene3d.lineSegment (Scene3d.Material.color Color.lightGreen)
                --     (LineSegment3d.along Axis3d.y (Length.meters 0) (Length.meters 10))
                :: List.map viewEntity (Physics.World.bodies model.physicsWorld)
        , shadows = True
        , upDirection = Direction3d.positiveZ
        , sunlightDirection = Direction3d.yz (Angle.degrees -120)
        }


viewEntity : Body Entity -> Scene3d.Entity WorldCoordinates
viewEntity body =
    case Physics.Body.data body of
        CannonBall _ ->
            Scene3d.sphereWithShadow
                (Scene3d.Material.metal { baseColor = Color.black, roughness = 0.5 })
                (Sphere3d.atPoint Point3d.origin (Length.meters 0.25)
                    |> Sphere3d.placeIn (Physics.Body.frame body)
                )

        Ground _ ->
            Scene3d.nothing

        WallPermanent _ block ->
            Scene3d.blockWithShadow
                (Scene3d.Material.metal { baseColor = Color.gray, roughness = 0.5 })
                block

        ETank _ tank ->
            viewTank body tank


viewTank : Body Entity -> Tank -> Scene3d.Entity WorldCoordinates
viewTank body tank =
    let
        bodyPosition : Frame3d.Frame3d Meters WorldCoordinates { defines : Physics.Coordinates.BodyCoordinates }
        bodyPosition =
            Physics.Body.frame body

        topPosition : Frame3d.Frame3d Meters WorldCoordinates defines2
        topPosition =
            bodyPosition
                |> Frame3d.translateIn Direction3d.positiveZ (Length.meters 0.25)
    in
    Scene3d.group
        [ Scene3d.blockWithShadow
            tank.material
            (Block3d.centeredOn bodyPosition
                ( Length.meters 2.0
                , Length.meters 1.0
                , Length.meters 0.5
                )
            )
        , Scene3d.cylinderWithShadow
            tank.material
            (Cylinder3d.startingAt
                Point3d.origin
                Direction3d.positiveZ
                { radius = Length.meters 0.5
                , length = Length.meters 0.25
                }
                |> Cylinder3d.placeIn topPosition
            )
        , Scene3d.cylinderWithShadow
            tank.material
            (Cylinder3d.startingAt
                (Point3d.meters 0.5 0.0 0.125)
                Direction3d.positiveX
                { radius = Length.meters 0.125
                , length = Length.meters 1.0
                }
                |> Cylinder3d.rotateAround Axis3d.y tank.cannonPitch
                |> Cylinder3d.rotateAround Axis3d.z tank.cannonRotation
                |> Cylinder3d.placeIn topPosition
            )
        ]
