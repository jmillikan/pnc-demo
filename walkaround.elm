import Html exposing (Html, button, div, img, text)
import Html.Events exposing (onClick, onWithOptions, Options)
import Html.Attributes exposing (style, src)
import Time exposing (Time, millisecond)
import AnimationFrame exposing (diffs)
import Json.Decode as Json
import Keyboard exposing (KeyCode, presses)
import Char exposing (toCode)
import Maybe exposing (andThen)

main : Program Never State Msg
main = Html.program { init = (init, Cmd.none), view = view, update = update, subscriptions = subscriptions }

init : State
init = State initChar demoScene False

initChar : Character
initChar = (Character 120 180 (Pos 200 300) Still (0.2 / millisecond) Right [("1", 500 * millisecond), ("2", 500 * millisecond)])       

-- demoScene isn't as exciting as it sounds.
demoScene : Scene
demoScene = Scene
            "bg1"
            [Playfield 800 330 100 160, Playfield 510 100 890 350, Playfield 700 100 -590 280]
            [Pos 1100 400] 
            [Exit (Playfield 100 300 1090 150) (Pos 1140 400) 1 0]

scene2 : Scene
scene2 = Scene
         "bg2"
         [Playfield 200 100 0 150, Playfield 800 500 130 100]
         [Pos 50 200]
         [Exit (Playfield 70 200 0 50) (Pos 50 200) 0 0]

scenes : List Scene
scenes = [demoScene, scene2]

-- Much later we might need ticks always or more of the time...
-- For now I just don't want the extra history in reactor
subscriptions : State -> Sub Msg
subscriptions model = Sub.batch [ presses Key
                                , case model.character.state of
                                      Still -> Sub.none
                                      MovingTo _ _ _ -> diffs Tick
                                ]

type alias State = { character : Character
                   , scene : Scene
                   , debug : Bool
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
                       , walkCycle : AnimCycle
                       }

type Facing = Left | Right    

-- MovingTo - multi-segment movement plan
type CharState = Still | MovingTo (List Pos) InAnimation Action

type Action = None | Leave Exit

type alias AnimCycle = List (String, Time)

-- Very simple animation system
-- Or it was supposed to be...
type alias InAnimation = { segments : AnimCycle
                         , current : AnimCycle
                         }

type alias Scene = { image : String
                   , playfields : List Playfield
                   , entrance : List Pos
                   , exits : List Exit
                   }

-- Playfield segment rectangle
-- Needs to be called something else...
type alias Playfield = { width : Float
                       , height : Float
                       , x : Float
                       , y : Float
                       }

type alias Exit = { field : Playfield
                  , position : Pos
                  , destination : Int
                  , destinationSpawn : Int
                  }

type Msg = Tick Time | FloorClick Int Int | ExitClick Int Int | Key KeyCode

update : Msg -> State -> ( State, Cmd Msg )    
update msg model =
    let char = model.character in
    case msg of
        Key p -> (if p == toCode 'd' then { model | debug = not model.debug } else model, Cmd.none)
        Tick delta ->
            let (newChar, action) = walk char delta -- In the future, maybe things can happen other ways... ie ambient timers or animations
                newState = { model | character = newChar }
            in case action of
                   Nothing -> (newState, Cmd.none)
                   Just a -> (doAction a newState, Cmd.none)
        ExitClick x y ->
            case findExit model.scene.exits (Pos (toFloat x) (toFloat y)) of
                Nothing -> (Debug.log "Bad exit click?" model, Cmd.none)
                Just exit ->
                    let ps = walkOneX char.pos model.scene.playfields exit.position
                        newState = case ps of
                                       Nothing -> Debug.log "Failed exit click..." Still
                                       Just ps -> MovingTo ps (InAnimation char.walkCycle char.walkCycle) (Leave exit)
                    in ({ model | character = { char | state = newState } }, Cmd.none)
        FloorClick x y ->
            let newState = 
                    case walkOneX char.pos model.scene.playfields (Pos (toFloat x) (toFloat y))
                    of
                        Nothing -> Still
                        Just ps -> MovingTo ps (InAnimation char.walkCycle char.walkCycle) None
            in ({ model | character = { char | state = newState } }, Cmd.none)

doAction : Action -> State -> State
doAction action model =
    case action of
        None -> model
        Leave exit ->
            let scene = List.head <| List.drop exit.destination scenes -- Need to use a Dict...
                spawn = andThen (List.head << List.drop exit.destinationSpawn << .entrance) scene
            in case (scene, spawn) of 
                   (Just newScene, Just spawn) ->
                       { model | scene = newScene, character =
                             let char = model.character in { char | pos = spawn } } -- TODO: Position character...
                   _ -> Debug.log "Missing scene" model

-- Pos isn't a great representation for a direction, oh well
directionFrom : Pos -> Pos -> Pos
directionFrom p1 p2 = let dx = p2.x - p1.x
                          dy = p2.y - p1.y
                          dist = distanceFrom p1 p2 in
                      if dist == 0 then Pos 0 0
                      else Pos (dx * (1 / dist)) (dy * (1 / dist))

distanceFrom : Pos -> Pos -> Float
distanceFrom p1 p2 = sqrt ((p1.x - p2.x) ^ 2 + (p1.y - p2.y) ^ 2)

-- Character's new position, and whether the character stopped on an exit                     
walk : Character -> Float -> (Character, Maybe Action)
walk char delta =
    case char.state of
        Still -> (char, Nothing)
        -- No more movement to do, hold still
        MovingTo [] _ a -> ({ char | state = Still }, Just a)
        -- Move toward dest at char.speed
        MovingTo (dest :: rest) anim action ->
            -- Do some fairly bad math to move (delta * char.speed) towards destination
            let maxDistance = delta * char.speed
                distance = distanceFrom char.pos dest
                newFacing = if dest.x > char.pos.x then Right else Left
            in
            if maxDistance > distance
            then walk { char | pos = Pos dest.x dest.y
                      , state = MovingTo rest (advanceAnim anim delta) action
                      , facing = newFacing } delta -- Give away some movement here instead of bothering to chop up delta
            else
                let dir = directionFrom char.pos dest
                in ({ char | pos = Pos (char.pos.x + maxDistance * dir.x) (char.pos.y + maxDistance * dir.y)
                    , state = MovingTo (dest :: rest) (advanceAnim anim delta) action
                    , facing = newFacing
                    }, Nothing)

-- Diverges if: a.segments is empty, any segment time is 0 or negative, ???              
advanceAnim : InAnimation -> Time -> InAnimation
advanceAnim a delta =
    case Debug.log "current" a.current of
        [] -> advanceAnim (InAnimation a.segments a.segments) delta
        (pose, remaining) :: rest ->
            if remaining > delta
            then (InAnimation a.segments ((pose, remaining - delta) :: rest))
            else advanceAnim (InAnimation a.segments rest) (delta - remaining)

-- Plan a move to (x, y) through playfield graph.
--startWalking : Character -> Pos -> Character                    
--startWalking char p = { char | state = MovingTo [p] (InAnimation char.walkCycle char.walkCycle) }

-- First attempt at 'pathfinding' - keep just the field list, route through max of one "intersection" by force
-- Next attempt should use BFS^wtree-spanning
walkOneX : Pos -> List Playfield -> Pos -> Maybe (List Pos)
walkOneX sourceP playfields p =
    let srcPlayfield = findPlayfield playfields sourceP
        -- Really we could get dest through click events but that's no fun
        destPlayfield = findPlayfield playfields p
    in
    case (srcPlayfield, destPlayfield, srcPlayfield == destPlayfield) of
        (Just src, Just dest, True) -> Just [p]
        (Just src, Just dest, False) ->
            case middleOfX src dest of
                Just firstX -> Just [firstX, p]
                _ -> Debug.log "No middleOfX" Nothing -- We'll be getting this until I fix routing
        _ -> Debug.log "No src or no dest" Nothing -- When a click or the character is outside any known field...
                     
findPlayfield : List Playfield -> Pos -> Maybe Playfield
findPlayfield fields pos = List.head <| List.filter (pointInPlayfield pos) fields

findExit : List Exit -> Pos -> Maybe Exit
findExit exits pos = List.head <| List.filter (pointInPlayfield pos << .field) exits

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
    in let l = max f1.x f2.x
           r = min f1right f2right
           t = max f1.y f2.y
           b = min f1bottom f2bottom
       in
           if l <= r && t <= b
           then Just <| Pos ((l + r) / 2) ((t + b) / 2)
           else Nothing

-- Render a background image and character
-- Optionally render some debug geometry...
-- For now the view is fixed in scale... Someday, scale it along with click events to accomodate
-- the poor, miserable wretches who don't have my specific laptop
view : State -> Html Msg
view model =
  div [ style [ ("background-image", "url(img/" ++ model.scene.image ++ ".png)")
              , ("width", "1666px")
              , ("height", "724px")
              ]
      ]
      [ div [] (if model.debug then (List.map (viewDebugField "blue") model.scene.playfields)
                    ++ (List.map (viewDebugField "green" << .field) model.scene.exits)
                    ++ (List.map (viewDebugPos "e" << .position) model.scene.exits) else [])
      , viewChar model.character 
      , div [] (if model.debug then [viewDebugPos "C" model.character.pos] else [])
      -- These need to be "on top". This is not really a good solution.
      , div [] (List.map (clickField identity (always FloorClick)) model.scene.playfields)
      , div [] (List.map (clickField (.field) (always ExitClick)) model.scene.exits)
      ]

selfish : Options
selfish = Options True True

-- https://github.com/fredcy/elm-svg-mouse-offset/blob/master/Main.elm    
offsetPosition : (Int -> Int -> Msg) -> Json.Decoder Msg
offsetPosition msg = Json.map2 msg (Json.field "pageX" Json.int) (Json.field "pageY" Json.int)

-- A lot of overlap with viewDebugField...                     
clickField : (a -> Playfield) -> (a -> Int -> Int -> Msg) -> a -> Html Msg
clickField f m e = div [ style [ ("height", toString (f e).height ++ "px")
                          , ("width", toString (f e).width ++ "px")
                          , ("position", "absolute")
                          , ("top", toString (f e).y ++ "px") -- fudge fudge
                          , ("left", toString (f e).x ++ "px")
                          ]
                  , onWithOptions "click" selfish (offsetPosition (m e)) ] [ ]

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
             MovingTo _ a _ ->
                 case a.current of
                     (p, _) :: _ -> p
                     _ -> "1"
                               
face : Character -> String
face c = case c.facing of
             Left -> "left"
             Right -> "right"

viewDebugPos : String -> Pos -> Html Msg
viewDebugPos c p = div
                  [ style [ ("position", "absolute")
                          , ("top", toString p.y ++ "px") -- fudge fudge
                          , ("left", toString p.x ++ "px")
                          , ("border", "solid red")
                          , ("border-width", "2px 0px 0px 2px")
                          ] ]
                  [ text c ]
             
viewDebugField : String -> Playfield -> Html Msg             
viewDebugField color f = div [ style [ ("background-color", color)
                               , ("height", toString f.height ++ "px")
                               , ("width", toString f.width ++ "px")
                               , ("position", "absolute")
                               , ("top", toString f.y ++ "px")
                               , ("left", toString f.x ++ "px")
                               ] ] [ text "field" ]
