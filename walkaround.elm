import Html exposing (Html, button, div, img, text)
import Html.Events exposing (onClick, on)
import Html.Attributes exposing (style, src)
import Time exposing (Time, millisecond)
import AnimationFrame exposing (diffs)
import Json.Decode as Json

main : Program Never State Msg
main = Html.program { init = (init, Cmd.none), view = view, update = update, subscriptions = subscriptions }

-- Start time of 0 means the first time-delta will be a whopper. Make sure nothing's delta-dependent on the first step.
init : State
init = State initChar demoScene

initChar : Character
initChar = (Character 120 180 (Pos 200 300) Still (0.2 / millisecond) Right [("1", 500 * millisecond), ("2", 500 * millisecond)])       

-- demoScene isn't as exciting as it sounds.
demoScene : List Playfield
demoScene = [Playfield 800 330 100 160, Playfield 510 100 890 350, Playfield 300 100 -100 280]

-- Much later we might need ticks always or more of the time...
-- For now I just don't want the extra history in reactor
subscriptions : State -> Sub Msg
subscriptions model = case model.character.state of
                          Still -> Sub.none
                          MovingTo _ _ -> diffs Tick

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
                       , facing : Facing -- This is separate from walkCycle for now...
                       , walkCycle : List (String, Time)
                       }

type Facing = Left | Right    

-- MovingTo - multi-segment movement plan
type CharState = Still | MovingTo (List Pos) AnimCycle

-- Very simple animation system
-- Or it was supposed to be...
type alias AnimCycle = { segments : List (String, Time)
                       , current : List (String, Time)
                       }

-- Playfield segment rectangle
-- Needs to be called something else...
type alias Playfield = { width : Float
                       , height : Float
                       , x : Float
                       , y : Float
                       }

type Msg = Tick Time | Click Int Int

update : Msg -> State -> ( State, Cmd Msg )    
update msg model =
    let char = model.character in
    case msg of
        Tick delta ->
            (State (walk char delta) model.playfield, Cmd.none)
        Click x y ->
            let newState = 
                    case walkOneX char.pos model.playfield (Pos (toFloat x) (toFloat y))
                    of
                        Nothing -> Still
                        Just ps -> MovingTo ps (AnimCycle char.walkCycle char.walkCycle)
            in (State { char | state = newState } model.playfield, Cmd.none)

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
        MovingTo [] _ -> { char | state = Still }
        -- Move toward dest at char.speed
        MovingTo (dest :: rest) anim ->
            -- Do some fairly bad math to move (delta * char.speed) towards destination
            let maxDistance = delta * char.speed
                distance = distanceFrom char.pos dest
                newFacing = if dest.x > char.pos.x then Right else Left
            in
            if maxDistance > distance
            then walk { char | pos = Pos dest.x dest.y
                      , state = MovingTo rest (advanceAnim anim delta)
                      , facing = newFacing } delta -- Give away some movement here instead of bothering to chop up delta
            else
                let dir = directionFrom char.pos dest
                in { char | pos = Pos (char.pos.x + maxDistance * dir.x) (char.pos.y + maxDistance * dir.y)
                   , state = MovingTo (dest :: rest) (advanceAnim anim delta)
                   , facing = newFacing
                   }

-- Diverges if: a.segments is empty, any segment time is 0 or negative, ???              
advanceAnim : AnimCycle -> Time -> AnimCycle
advanceAnim a delta =
    case Debug.log "current" a.current of
        [] -> advanceAnim (AnimCycle a.segments a.segments) delta
        (pose, remaining) :: rest ->
            if remaining > delta
            then (AnimCycle a.segments ((pose, remaining - delta) :: rest))
            else advanceAnim (AnimCycle a.segments rest) (delta - remaining)

-- Plan a move to (x, y) through playfield graph.
--startWalking : Character -> Pos -> Character                    
--startWalking char p = { char | state = MovingTo [p] (AnimCycle char.walkCycle char.walkCycle) }

-- First attempt at 'pathfinding' - keep just the field list, route through max of one "intersection" by force
-- Next attempt should use BFS...
walkOneX : Pos -> List Playfield -> Pos -> Maybe (List Pos)
walkOneX sourceP playfields p =
    let srcPlayfield = findPlayfield playfields sourceP
        -- Really we could get dest through click events but that's no fun
        destPlayfield = findPlayfield playfields p in
    case (srcPlayfield, destPlayfield, srcPlayfield == destPlayfield) of
        (Just src, Just dest, True) -> Just [p]
        (Just src, Just dest, False) ->
            case middleOfX src dest of
                Just firstX -> Just [firstX, p]
                _ -> Debug.log "No middleOfX" Nothing
        _ -> Debug.log "No src or no dest" Nothing
                     
findPlayfield : List Playfield -> Pos -> Maybe Playfield
findPlayfield fields pos = List.head <| List.filter (pointInPlayfield pos) fields

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
  div [ on "click" offsetPosition
      , style [ ("background-image", "url(img/bg1.png)")
              , ("width", "1666px")
              , ("height", "724px")
              ]
      ]
      [ viewChar <| model.character
      -- , div [] (List.map viewDebugField model.playfield) -- One hardcoded area to walk in...
      ]

-- https://github.com/fredcy/elm-svg-mouse-offset/blob/master/Main.elm    
offsetPosition : Json.Decoder Msg
offsetPosition = Json.map2 Click (Json.field "pageX" Json.int) (Json.field "pageY" Json.int)

viewChar : Character -> Html Msg
viewChar c = div [ style [ ("height", toString c.height ++ "px")
                         , ("width", toString c.width ++ "px")
                         , ("position", "absolute")
                         , ("top", toString (c.pos.y - c.height + 20) ++ "px") -- fudge fudge
                         , ("left", toString (c.pos.x - c.width / 2) ++ "px")
                         ] ] [ img [src ("img/" ++ pose c ++ face c ++ ".png"), style [ ("width", toString c.width ++ "px" ) ] ] [] ]

pose : Character -> String
pose c = case c.state of
             Still -> "s"
             MovingTo _ a ->
                 case a.current of
                     (p, _) :: _ -> p
                     _ -> "1"
                               
face : Character -> String
face c = case c.facing of
             Left -> "left"
             Right -> "right"

viewDebugField : Playfield -> Html Msg             
viewDebugField f = div [ style [ ("background-color", "green")
                               , ("height", toString f.height ++ "px")
                               , ("width", toString f.width ++ "px")
                               , ("position", "absolute")
                               , ("top", toString f.y ++ "px")
                               , ("left", toString f.x ++ "px")
                               ] ] [ text "field" ]
