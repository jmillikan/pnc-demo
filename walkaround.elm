import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick, on)
import Html.Attributes exposing (style)
import Time exposing (Time, millisecond)
import AnimationFrame exposing (diffs)
import Json.Decode as Json

main : Program Never State Msg
main = Html.program { init = (init, Cmd.none), view = view, update = update, subscriptions = subscriptions }

-- Start time of 0 means the first time-delta will be a whopper. Make sure nothing's delta-dependent on the first step.
init : State
init = State (Character 40 100 (Pos 200 300) Still (0.2 / millisecond)) [Playfield 800 400 100 100, Playfield 600 100 800 350]

subscriptions : State -> Sub Msg
subscriptions model = diffs Tick

type alias State = { character : Character
                   , playfield : List Playfield
                   }

-- Positions and units are screen pixels
type alias Pos = { x: Float, y: Float }

-- A character. Its position is assumed to be at the bottom left of its graphic
type alias Character = { width : Float
                       , height : Float
                       , pos : Pos
                       , state : CharState
                       , speed : Float -- pixels/Time in ms?
                       }

-- MovingTo - multi-segment movement plan
-- TODO: Walking animation...
type CharState = Still | MovingTo (List Pos)

-- Playfield segment rectangle
type alias Playfield = { width : Float
                       , height : Float
                       , x : Float
                       , y : Float
                       }

type Msg = Tick Time | Click Int Int

update : Msg -> State -> ( State, Cmd Msg )    
update msg model =
    case msg of
        Tick delta ->
            (State (walk model.character delta) model.playfield, Cmd.none)
        --Click x y -> (State (startWalking model.character (Pos (toFloat x) (toFloat y))) model.playfield, Cmd.none)
        Click x y -> (State (walkOneX model.character model.playfield (Pos (toFloat x) (toFloat y))) model.playfield, Cmd.none)

-- Pos isn't a great representation for a direction, oh well
directionFrom : Pos -> Pos -> Pos
directionFrom p1 p2 = let dx = p2.x - p1.x
                          dy = p2.y - p1.y
                          dist = distanceFrom p1 p2 in
                      if dist == 0 then Pos 0 0
                      else Pos (dx * (1 / dist)) (dy * (1 / dist))

distanceFrom : Pos -> Pos -> Float
distanceFrom p1 p2 = sqrt ((p1.x - p2.x) ^ 2 + (p1.y - p2.y) ^ 2)

walk : Character -> Float -> Character
walk char delta =
    case char.state of
        Still -> char
        -- No more movement to do, hold still
        MovingTo [] -> { char | state = Still }
        -- Move toward dest at char.speed
        MovingTo (dest :: rest) ->
            -- Do some fairly bad math to move (delta * char.speed) towards destination
            let maxDistance = delta * char.speed
                distance = distanceFrom char.pos dest in
            if maxDistance > distance
            then walk { char | pos = Pos dest.x dest.y, state = MovingTo rest } delta -- Give away some movement here instead of bothering to chop up delta
            else
                let dir = directionFrom char.pos dest
                in { char | pos = Pos (char.pos.x + maxDistance * dir.x) (char.pos.y + maxDistance * dir.y) }

-- Plan a move to (x, y) through playfield graph.
startWalking : Character -> Pos -> Character                    
startWalking char p = { char | state = MovingTo [p] }

-- First attempt at 'pathfinding' - keep just the field list, route through max of one "intersection" by force
-- Next attempt should use BFS...
walkOneX : Character -> List Playfield -> Pos -> Character
walkOneX char playfields p =
    let srcPlayfield = findPlayfield playfields char.pos
        -- Really we could get dest through click events but that's no fun
        destPlayfield = findPlayfield playfields p in
    case (srcPlayfield, destPlayfield, srcPlayfield == destPlayfield) of
        (Just src, Just dest, True) -> { char | state = MovingTo [p] }
        (Just src, Just dest, False) ->
            case middleOfX src dest of
                Just firstX -> { char | state = MovingTo [firstX, p] }
                _ -> Debug.log "No middleOfX for " char
        _ -> Debug.log "No src or no dest for " char
                     
findPlayfield : List Playfield -> Pos -> Maybe Playfield
findPlayfield fields pos =
    case List.filter (pointInPlayfield pos) fields of
        [] -> Nothing
        f :: _ -> Just f

pointInPlayfield : Pos -> Playfield -> Bool
pointInPlayfield p field =
    p.x >= field.x
        && p.x <= field.x + field.width
        && p.y >= field.y
        && p.y <= field.y + field.height

-- Middle of intersection of two fields, or Nothing if not intersecting
middleOfX : Playfield -> Playfield -> Maybe Pos
middleOfX f1 f2 =
    let f1right = f1.x + f1.width
        f1bottom = f1.y + f1.height
        f2right = f2.x + f2.width
        f2bottom = f2.y + f2.height 
    in
        let l = max f1.x f2.x
            r = min f1right f2right
            t = max f1.y f2.y
            b = min f1bottom f2bottom
        in
            if l <= r && t <= b
            then
                Just <| Pos ((l + r) / 2) ((t + b) / 2)
            else
                Nothing

-- Render everything with plain absolute divs for now.
view : State -> Html Msg
view model =
  div [ on "click" offsetPosition ]
      [ div [] (List.map viewField model.playfield) -- One hardcoded area to walk in...
      , viewChar <| model.character
      ]

-- https://github.com/fredcy/elm-svg-mouse-offset/blob/master/Main.elm    
offsetPosition : Json.Decoder Msg
offsetPosition = Json.map2 Click (Json.field "pageX" Json.int) (Json.field "pageY" Json.int)

viewChar : Character -> Html Msg
viewChar c = div [ style [ ("background-color", "red")
                         , ("height", toString c.height ++ "px")
                         , ("width", toString c.width ++ "px")
                         , ("position", "absolute")
                         , ("top", toString (c.pos.y - c.height) ++ "px")
                         , ("left", toString c.pos.x ++ "px")
                         ] ] [ text "guy" ]

viewField : Playfield -> Html Msg             
viewField f = div [ style [ ("background-color", "green")
                         , ("height", toString f.height ++ "px")
                         , ("width", toString f.width ++ "px")
                         , ("position", "absolute")
                         , ("top", toString f.y ++ "px")
                         , ("left", toString f.x ++ "px")
                         ] ] [ text "field" ]
