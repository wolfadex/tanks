module Main exposing (main)

import Acceleration
import Angle exposing (Angle)
import Axis3d
import Block3d exposing (Block3d)
import Browser exposing (Document)
import Browser.Dom
import Browser.Events exposing (Visibility(..))
import Camera3d exposing (Camera3d)
import Color
import Cylinder3d
import Direction2d
import Direction3d
import Duration
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Force
import Frame3d
import Html exposing (Html)
import Json.Decode exposing (Value)
import Length exposing (Meters)
import Mass
import Physics.Body exposing (Body)
import Physics.Contact
import Physics.Coordinates exposing (WorldCoordinates)
import Physics.World exposing (World)
import Pixels
import Point3d
import Quantity
import Scene3d
import Scene3d.Material
import SketchPlane3d
import Sphere3d
import Task
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
    , showControls : Visibility
    , windowSize : { width : Int, height : Int }
    , windowVisible : Visibility
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
    = Ground
    | CannonBall
    | WallPermanent (Block3d Meters WorldCoordinates)
    | ETank Int Tank Int
    | EBunker Int Bunker Int


type KeyPressesd
    = Pressed
    | Unpressed


type alias Tank =
    { cannonRotation : Angle
    , cannonPitch : Angle
    , material : Scene3d.Material.Uniform WorldCoordinates
    }


type alias Bunker =
    { cannonRotation : Angle
    , lastCannonFiredAt : Float
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( { elapsedTime = 0
      , showControls = Hidden
      , windowSize = { width = 800, height = 600 }
      , windowVisible = Visible
      , tank =
            { cannonRotation = Angle.degrees 0
            , cannonPitch = Angle.degrees 0
            , material = Scene3d.Material.metal { baseColor = Color.lightBlue, roughness = 0.5 }
            }
      , playerId = 0
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
                |> Physics.World.add (Physics.Body.plane Ground)
                -- Inner walls
                |> Physics.World.add
                    (let
                        block : Block3d Meters coordinates
                        block =
                            Block3d.from (Point3d.meters 10 4 0) (Point3d.meters 11 -4 2)
                     in
                     Physics.Body.block block (WallPermanent block)
                    )
                |> Physics.World.add
                    (let
                        block : Block3d Meters coordinates
                        block =
                            Block3d.from (Point3d.meters 4 -10 0) (Point3d.meters -4 -11 2)
                     in
                     Physics.Body.block block (WallPermanent block)
                    )
                |> Physics.World.add
                    (let
                        block : Block3d Meters coordinates
                        block =
                            Block3d.from (Point3d.meters -10 -4 0) (Point3d.meters -11 4 2)
                     in
                     Physics.Body.block block (WallPermanent block)
                    )
                |> Physics.World.add
                    (let
                        block : Block3d Meters coordinates
                        block =
                            Block3d.from (Point3d.meters -4 10 0) (Point3d.meters 4 11 2)
                     in
                     Physics.Body.block block (WallPermanent block)
                    )
                -- Outer walls
                |> Physics.World.add
                    (let
                        block : Block3d Meters coordinates
                        block =
                            Block3d.from (Point3d.meters 20 21 0) (Point3d.meters 21 -21 2)
                     in
                     Physics.Body.block block (WallPermanent block)
                    )
                |> Physics.World.add
                    (let
                        block : Block3d Meters coordinates
                        block =
                            Block3d.from (Point3d.meters 21 -20 0) (Point3d.meters -21 -21 2)
                     in
                     Physics.Body.block block (WallPermanent block)
                    )
                |> Physics.World.add
                    (let
                        block : Block3d Meters coordinates
                        block =
                            Block3d.from (Point3d.meters -20 -21 0) (Point3d.meters -21 21 2)
                     in
                     Physics.Body.block block (WallPermanent block)
                    )
                |> Physics.World.add
                    (let
                        block : Block3d Meters coordinates
                        block =
                            Block3d.from (Point3d.meters -21 20 0) (Point3d.meters 21 21 2)
                     in
                     Physics.Body.block block (WallPermanent block)
                    )
                |> Physics.World.add
                    (Physics.Body.block
                        (Block3d.centeredOn Frame3d.atOrigin
                            ( Length.meters 2.0
                            , Length.meters 1.0
                            , Length.meters 0.5
                            )
                        )
                        (ETank
                            0
                            { cannonRotation = Angle.degrees 0
                            , cannonPitch = Angle.degrees 0
                            , material = Scene3d.Material.metal { baseColor = Color.lightBlue, roughness = 0.5 }
                            }
                            1
                        )
                        |> Physics.Body.withBehavior (Physics.Body.dynamic (Mass.kilograms 1000))
                    )
                |> Physics.World.add
                    (Physics.Body.sphere
                        (Sphere3d.atPoint Point3d.origin (Length.meters 1.0))
                        (EBunker
                            0
                            { cannonRotation = Angle.degrees 0
                            , lastCannonFiredAt = 0
                            }
                            1
                        )
                        |> Physics.Body.moveTo (Point3d.meters 15 0 0)
                        |> Physics.Body.withBehavior Physics.Body.static
                    )
                |> Physics.World.add
                    (Physics.Body.sphere
                        (Sphere3d.atPoint Point3d.origin (Length.meters 1.0))
                        (EBunker
                            1
                            { cannonRotation = Angle.degrees 0
                            , lastCannonFiredAt = 0
                            }
                            1
                        )
                        |> Physics.Body.moveTo (Point3d.meters -15 0 0)
                        |> Physics.Body.withBehavior Physics.Body.static
                    )
      }
    , Browser.Dom.getViewport
        |> Task.perform
            (\viewport ->
                WindowResize (floor viewport.viewport.width) (floor viewport.viewport.height)
            )
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.windowVisible of
        Hidden ->
            Browser.Events.onVisibilityChange WindowVisibilityChange

        Visible ->
            Sub.batch
                [ Browser.Events.onKeyDown (Json.Decode.map KeyDown Json.Decode.value)
                , Browser.Events.onKeyUp (Json.Decode.map KeyUp Json.Decode.value)
                , Browser.Events.onAnimationFrameDelta Tick
                , Browser.Events.onResize WindowResize
                , Browser.Events.onVisibilityChange WindowVisibilityChange
                ]


type Msg
    = Tick Float
    | KeyDown Value
    | KeyUp Value
    | WindowResize Int Int
    | WindowVisibilityChange Visibility
    | ToggleControls


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
        maybePlayerTank : Maybe ( Body Entity, Tank )
        maybePlayerTank =
            Physics.World.bodies model.physicsWorld
                |> findInList
                    (\body ->
                        let
                            data =
                                Physics.Body.data body
                        in
                        case data of
                            ETank id tankData _ ->
                                if id == model.playerId then
                                    Just ( body, tankData )

                                else
                                    Nothing

                            _ ->
                                Nothing
                    )

        ( cannonBallMaybeAdded, newLastCannonBallFireTime ) =
            case model.fireCannon of
                Pressed ->
                    if model.elapsedTime - model.lastCannonFiredAt > 100 then
                        case maybePlayerTank of
                            Nothing ->
                                ( model.physicsWorld, model.lastCannonFiredAt )

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
                                                CannonBall
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
                                )

                    else
                        ( model.physicsWorld, model.lastCannonFiredAt )

                Unpressed ->
                    ( model.physicsWorld, model.lastCannonFiredAt )

        ( worldWithEnemyCannonBalls, bunkersThatFired ) =
            case maybePlayerTank of
                Nothing ->
                    ( cannonBallMaybeAdded, [] )

                Just ( playerTankBody, _ ) ->
                    cannonBallMaybeAdded
                        |> Physics.World.bodies
                        |> List.filterMap
                            (\body ->
                                case Physics.Body.data body of
                                    EBunker bunkerId bunker _ ->
                                        let
                                            targetPoint3d =
                                                Physics.Body.frame playerTankBody
                                                    |> Frame3d.originPoint

                                            bunkerPoint3d =
                                                Physics.Body.frame body
                                                    |> Frame3d.originPoint

                                            targetPoint2d =
                                                Point3d.projectInto SketchPlane3d.xy targetPoint3d

                                            bunkerPoint2d =
                                                Point3d.projectInto SketchPlane3d.xy bunkerPoint3d

                                            raycast =
                                                case Direction3d.from bunkerPoint3d targetPoint3d of
                                                    Nothing ->
                                                        Nothing

                                                    Just dir ->
                                                        Physics.World.raycast (Axis3d.through bunkerPoint3d dir) model.physicsWorld
                                        in
                                        case ( Direction2d.from bunkerPoint2d targetPoint2d, raycast ) of
                                            ( Just dirToTarget, Just raycastResult ) ->
                                                case Physics.Body.data raycastResult.body of
                                                    ETank id _ _ ->
                                                        if id == model.playerId then
                                                            let
                                                                normalizedTargetAngle : Angle
                                                                normalizedTargetAngle =
                                                                    Angle.normalize (Direction2d.toAngle dirToTarget)

                                                                normalizedCannonAngle : Angle
                                                                normalizedCannonAngle =
                                                                    Angle.normalize bunker.cannonRotation

                                                                angleDifference : Angle
                                                                angleDifference =
                                                                    Quantity.difference normalizedTargetAngle normalizedCannonAngle
                                                            in
                                                            if Quantity.greaterThan (Quantity.abs angleDifference) (Angle.degrees 15) && model.elapsedTime - bunker.lastCannonFiredAt > 2000 then
                                                                let
                                                                    position : Frame3d.Frame3d Meters WorldCoordinates { defines : Physics.Coordinates.BodyCoordinates }
                                                                    position =
                                                                        Physics.Body.frame body
                                                                            |> Frame3d.translateAlongOwn Frame3d.zAxis (Length.meters 0.5)
                                                                            |> Frame3d.rotateAroundOwn Frame3d.zAxis bunker.cannonRotation
                                                                            |> Frame3d.translateAlongOwn Frame3d.xAxis (Length.meters 1.6)

                                                                    cannonBallBody : Body Entity
                                                                    cannonBallBody =
                                                                        Physics.Body.sphere (Sphere3d.atPoint Point3d.origin (Length.meters 0.25)) CannonBall
                                                                            |> Physics.Body.moveTo (Frame3d.originPoint position)
                                                                            |> Physics.Body.withBehavior (Physics.Body.dynamic (Mass.kilograms 5.5))

                                                                    cannonBallEntity : Body Entity
                                                                    cannonBallEntity =
                                                                        Physics.Body.applyImpulse
                                                                            (Force.newtons 150 |> Quantity.times (Duration.seconds 0.5))
                                                                            (Frame3d.xDirection position)
                                                                            (Physics.Body.originPoint cannonBallBody)
                                                                            cannonBallBody
                                                                in
                                                                Just
                                                                    ( cannonBallEntity, bunkerId )

                                                            else
                                                                Nothing

                                                        else
                                                            Nothing

                                                    _ ->
                                                        Nothing

                                            _ ->
                                                Nothing

                                    _ ->
                                        Nothing
                            )
                        |> List.foldl
                            (\( cannonBall, bunkerId ) ( world, bunkers ) ->
                                ( Physics.World.add cannonBall world, bunkerId :: bunkers )
                            )
                            ( cannonBallMaybeAdded, [] )

        isCannonBall : Body Entity -> Bool
        isCannonBall body =
            case Physics.Body.data body of
                CannonBall ->
                    True

                _ ->
                    False

        simulatedWorld : World Entity
        simulatedWorld =
            Physics.World.simulate (Duration.milliseconds deltaMs) worldWithEnemyCannonBalls

        contacts : List (Physics.Contact.Contact Entity)
        contacts =
            Physics.World.contacts simulatedWorld

        updatedWorld : World Entity
        updatedWorld =
            simulatedWorld
                |> Physics.World.update
                    (\body ->
                        case Physics.Body.data body of
                            ETank id tank health ->
                                Physics.Body.withData
                                    (ETank
                                        id
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
                                        (if List.any isCannonBall (isContactedBy body contacts) then
                                            0

                                         else
                                            health
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

                            EBunker bunkerId bunker health ->
                                if List.any isCannonBall (isContactedBy body contacts) then
                                    Physics.Body.withData
                                        (EBunker
                                            bunkerId
                                            bunker
                                            (if List.any isCannonBall (isContactedBy body contacts) then
                                                0

                                             else
                                                health
                                            )
                                        )
                                        body

                                else
                                    case maybePlayerTank of
                                        Nothing ->
                                            body

                                        Just ( playerTankBody, _ ) ->
                                            let
                                                targetPoint3d =
                                                    Physics.Body.frame playerTankBody
                                                        |> Frame3d.originPoint

                                                bunkerPoint3d =
                                                    Physics.Body.frame body
                                                        |> Frame3d.originPoint

                                                targetPoint2d =
                                                    Point3d.projectInto SketchPlane3d.xy targetPoint3d

                                                bunkerPoint2d =
                                                    Point3d.projectInto SketchPlane3d.xy bunkerPoint3d

                                                raycast =
                                                    case Direction3d.from bunkerPoint3d targetPoint3d of
                                                        Nothing ->
                                                            Nothing

                                                        Just dir ->
                                                            Physics.World.raycast (Axis3d.through bunkerPoint3d dir) model.physicsWorld
                                            in
                                            case ( Direction2d.from bunkerPoint2d targetPoint2d, raycast ) of
                                                ( Just dirToTarget, Just raycastResult ) ->
                                                    case Physics.Body.data raycastResult.body of
                                                        ETank id _ _ ->
                                                            if id == model.playerId then
                                                                let
                                                                    normalizedTargetAngle : Angle
                                                                    normalizedTargetAngle =
                                                                        Angle.normalize (Direction2d.toAngle dirToTarget)

                                                                    normalizedCannonAngle : Angle
                                                                    normalizedCannonAngle =
                                                                        Angle.normalize bunker.cannonRotation

                                                                    angleDifference : Angle
                                                                    angleDifference =
                                                                        Quantity.difference normalizedTargetAngle normalizedCannonAngle

                                                                    maxToRotate : Angle
                                                                    maxToRotate =
                                                                        Angle.atan2 (Angle.radians (Angle.sin angleDifference)) (Angle.radians (Angle.cos angleDifference))

                                                                    toRotate : Angle
                                                                    toRotate =
                                                                        if Angle.inDegrees maxToRotate < 0 then
                                                                            Quantity.max (Angle.degrees -0.5) maxToRotate

                                                                        else
                                                                            Quantity.min (Angle.degrees 0.5) maxToRotate
                                                                in
                                                                Physics.Body.withData
                                                                    (EBunker
                                                                        bunkerId
                                                                        { bunker
                                                                            | cannonRotation =
                                                                                Quantity.plus normalizedCannonAngle toRotate
                                                                            , lastCannonFiredAt =
                                                                                if List.member bunkerId bunkersThatFired then
                                                                                    model.elapsedTime + deltaMs

                                                                                else
                                                                                    bunker.lastCannonFiredAt
                                                                        }
                                                                        (if List.any isCannonBall (isContactedBy body contacts) then
                                                                            0

                                                                         else
                                                                            health
                                                                        )
                                                                    )
                                                                    body

                                                            else
                                                                body

                                                        _ ->
                                                            body

                                                _ ->
                                                    body

                            _ ->
                                body
                    )

        -- simulatedContacts : List (Physics.Contact.Contact Entity)
        -- simulatedContacts =
        --     Physics.World.contacts simulatedWorld
    in
    { model
        | elapsedTime = model.elapsedTime + deltaMs
        , lastCannonFiredAt = newLastCannonBallFireTime
        , physicsWorld =
            updatedWorld
                |> Physics.World.keepIf
                    (\body ->
                        case Physics.Body.data body of
                            Ground ->
                                True

                            WallPermanent _ ->
                                True

                            ETank _ _ health ->
                                health > 0

                            EBunker _ _ health ->
                                health > 0

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


isContactedBy : Body Entity -> List (Physics.Contact.Contact Entity) -> List (Body Entity)
isContactedBy body contacts =
    isContactedByHelper body contacts []


isContactedByHelper : Body Entity -> List (Physics.Contact.Contact Entity) -> List (Body Entity) -> List (Body Entity)
isContactedByHelper body contacts otherBodies =
    case contacts of
        [] ->
            otherBodies

        next :: rest ->
            let
                ( leftBody, rightBody ) =
                    Physics.Contact.bodies next
            in
            if body == leftBody then
                isContactedByHelper body rest (rightBody :: otherBodies)

            else if body == rightBody then
                isContactedByHelper body rest (leftBody :: otherBodies)

            else
                isContactedByHelper body rest otherBodies


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick deltaMs ->
            ( applyTick deltaMs model
            , Cmd.none
            )

        ToggleControls ->
            ( { model
                | showControls =
                    case model.showControls of
                        Visible ->
                            Hidden

                        Hidden ->
                            Visible
              }
            , Cmd.none
            )

        WindowVisibilityChange visible ->
            ( { model | windowVisible = visible }, Cmd.none )

        WindowResize width height ->
            ( { model | windowSize = { width = width, height = height } }, Cmd.none )

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
    , body =
        [ viewControls model
        ]
    }


game3dScene : Model -> Html Msg
game3dScene model =
    let
        maybePlayerBody : Maybe (Body Entity)
        maybePlayerBody =
            Physics.World.bodies model.physicsWorld
                |> findInList
                    (\body ->
                        case Physics.Body.data body of
                            ETank id _ _ ->
                                if model.playerId == id then
                                    Just body

                                else
                                    Nothing

                            _ ->
                                Nothing
                    )

        camera : Camera3d Meters WorldCoordinates
        camera =
            Camera3d.perspective
                { viewpoint =
                    Viewpoint3d.lookAt
                        { focalPoint =
                            case maybePlayerBody of
                                Just playerBody ->
                                    Physics.Body.originPoint playerBody

                                Nothing ->
                                    Point3d.origin
                        , eyePoint =
                            case maybePlayerBody of
                                Just playerBody ->
                                    Physics.Body.originPoint playerBody
                                        |> Point3d.translateBy (Vector3d.meters 20 20 30)

                                Nothing ->
                                    Point3d.meters 20 20 30
                        , upDirection = Direction3d.positiveZ
                        }
                , verticalFieldOfView = Angle.degrees 30
                }

        ground : Scene3d.Entity WorldCoordinates
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
        , dimensions = ( Pixels.int model.windowSize.width, Pixels.int (model.windowSize.width * 9 // 16) )
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
        CannonBall ->
            Scene3d.sphereWithShadow
                (Scene3d.Material.metal { baseColor = Color.black, roughness = 0.5 })
                (Sphere3d.atPoint Point3d.origin (Length.meters 0.25)
                    |> Sphere3d.placeIn (Physics.Body.frame body)
                )

        Ground ->
            Scene3d.nothing

        WallPermanent block ->
            Scene3d.blockWithShadow
                (Scene3d.Material.metal { baseColor = Color.gray, roughness = 0.5 })
                block

        ETank _ tank _ ->
            viewTank body tank

        EBunker _ bunker _ ->
            viewBunker body bunker


viewBunker : Body Entity -> Bunker -> Scene3d.Entity WorldCoordinates
viewBunker body bunker =
    let
        bodyPosition : Frame3d.Frame3d Meters WorldCoordinates { defines : Physics.Coordinates.BodyCoordinates }
        bodyPosition =
            Physics.Body.frame body

        topPosition : Frame3d.Frame3d Meters WorldCoordinates defines2
        topPosition =
            bodyPosition
                |> Frame3d.translateIn Direction3d.positiveZ (Length.meters 0.25)

        material =
            Scene3d.Material.metal { baseColor = Color.lightRed, roughness = 0.9 }
    in
    Scene3d.group
        [ Scene3d.sphereWithShadow
            material
            (Sphere3d.atPoint (Frame3d.originPoint bodyPosition) (Length.meters 1.0))
        , Scene3d.cylinderWithShadow
            material
            (Cylinder3d.startingAt
                (Point3d.meters 0.5 0.0 0.125)
                Direction3d.positiveX
                { radius = Length.meters 0.125
                , length = Length.meters 1.0
                }
                |> Cylinder3d.rotateAround Axis3d.z bunker.cannonRotation
                |> Cylinder3d.placeIn topPosition
            )
        ]


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


viewControls : Model -> Html Msg
viewControls model =
    layout
        [ Font.family [ Font.monospace ]
        ]
        (el
            [ inFront <|
                column
                    []
                    [ Input.button
                        [ Background.color (rgb 1 1 1)
                        , paddingXY 16 8
                        ]
                        { onPress = Just ToggleControls
                        , label = text "Controls"
                        }
                    , case model.showControls of
                        Visible ->
                            column
                                [ Background.color (rgb 1 1 1)
                                , paddingXY 16 8
                                , Border.rounded 8
                                , moveRight 4
                                , moveDown 4
                                ]
                                [ el [ Font.underline ] (text "Controls:")
                                , text "Drive Forward ------------ W"
                                , text "Drive Backward ----------- S"
                                , text "Turn Clockwise ----------- D"
                                , text "Turn Counter-Clockwise --- A"
                                , text "Cannon Clockwise --------- E"
                                , text "Cannon Counter-Clockwise - Q"
                                , text "Pitch Cannon Up ---------- R"
                                , text "Pitch Cannon Down -------- F"
                                , text "Fire Cannon -------------- Space"
                                ]

                        Hidden ->
                            none
                    ]
            ]
            (html (game3dScene model))
        )
