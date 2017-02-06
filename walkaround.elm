import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick, on)
import Html.Attributes exposing (style)
import Time exposing (Time, millisecond)
import Json.Decode as Json

main : Program Never State Msg
main = Html.program { init = (init, Cmd.none), view = view, update = update, subscriptions = subscriptions }

-- Start time of 0 means the first time-delta will be a whopper. Make sure nothing's delta-dependent on the first step.
init = State (Character 40 100 200 300 Still (0.2 / millisecond)) (Playfield 800 400 100 100) 0

-- "Note: this function is not for animation!"
-- Okay, will have to revisit animation...
subscriptions model = Time.every (50 * millisecond) Tick

type alias State = { character : Character
                   , playfield : Playfield
                   , lastTime : Time
                   }

-- A character. Its position is assumed to be at the bottom left of its graphic
type alias Character = { width : Float
                       , height : Float
                       , x : Float
                       , y : Float
                       , state : CharState
                       , speed : Float -- pixels/Time in ms?
                       }

type CharState = Still | MovingTo Float Float

-- A single hardcoded area to walk around
type alias Playfield = { width : Float
                       , height : Float
                       , x : Float
                       , y : Float
                       }

type Msg = Tick Time | Click Int Int

update msg model =
    case msg of
        Tick t ->
            let (newTime, delta) = (t, t - model.lastTime) in
            (State (walk model.character delta) model.playfield newTime, Cmd.none)
        Click x y -> (State (startWalking model.character (toFloat x) (toFloat y)) model.playfield model.lastTime, Cmd.none)

directionFrom (x1, y1) (x2, y2) = let dx = x2 - x1
                                      dy = y2 - y1
                                      dist = distanceFrom (x1, y1) (x2, y2) in
                                  if dist == 0 then (0, 0)
                                  else (dx * (1 / dist), dy * (1 / dist))

distanceFrom (x1, y1) (x2, y2) = sqrt ((x1 - x2) ^ 2 + (y1 - y2) ^ 2)

walk : Character -> Float -> Character
walk char delta =
    case char.state of
        Still -> char
        MovingTo destX destY ->
            let maxDistance = delta * char.speed
                distance = distanceFrom (char.x, char.y) (destX, destY) in
            if maxDistance > distance
            then { char | x = destX, y = destY, state = Still }
            else
                let (xDirection, yDirection) = directionFrom (char.x, char.y) (destX, destY)
                in { char | x = char.x + maxDistance * xDirection, y = char.y + maxDistance * yDirection }

startWalking : Character -> Float -> Float -> Character                    
startWalking char x y = { char | state = MovingTo x y }

view model =
  div [ on "click" offsetPosition ]
      [ viewField <| model.playfield -- One hardcoded area to walk in...
      , viewChar <| model.character
      ]

-- https://github.com/fredcy/elm-svg-mouse-offset/blob/master/Main.elm    
offsetPosition : Json.Decoder Msg
offsetPosition = Json.map2 Click (Json.field "pageX" Json.int) (Json.field "pageY" Json.int)

viewChar c = div [ style [ ("background-color", "red")
                         , ("height", toString c.height ++ "px")
                         , ("width", toString c.width ++ "px")
                         , ("position", "absolute")
                         , ("top", toString (c.y - c.height) ++ "px")
                         , ("left", toString c.x ++ "px")
                         ] ] [ text "guy" ]

viewField f = div [ style [ ("background-color", "green")
                         , ("height", toString f.height ++ "px")
                         , ("width", toString f.width ++ "px")
                         , ("position", "absolute")
                         , ("top", toString f.y ++ "px")
                         , ("left", toString f.x ++ "px")
                         ] ] [ text "field" ]
