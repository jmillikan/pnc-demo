import Html exposing (Html, button, div, img, text)
import Html.Events exposing (onClick, onWithOptions, Options)
import Html.Attributes exposing (style, src)
import Time exposing (Time, millisecond)
import AnimationFrame exposing (diffs)
import Json.Decode as Json
import Keyboard exposing (KeyCode, presses)
import Char exposing (toCode)
import Maybe exposing (andThen, withDefault)
import Dict exposing (Dict, fromList, toList, get, values)
import Tuple exposing (first, second)

main : Program Never State Msg
main = Html.program { init = (init, Cmd.none), view = view, update = update, subscriptions = subscriptions }

init : State
init = State initChar "middle" sceneDict False []

initChar : Character
initChar = Character 120 180 (Pos 200 300) Still (0.2 / millisecond) Right [("1", 500 * millisecond), ("2", 500 * millisecond)] []

-- demoScene isn't as exciting as it sounds.
demoScene : Scene
demoScene = Scene
            "bg1"
            [Playfield 800 330 100 160, Playfield 510 100 890 350, Playfield 700 100 -590 280]
            [Pos 1100 400] 
            [Exit (Playfield 100 300 1250 150) (Pos 1140 400) "east" 0 "e-resize"
            ,Exit (Playfield 100 300 0 80) (Pos 50 330) "west" 0 "w-resize"]
            (fromList [])

scene2 : Scene
scene2 = Scene
         "bg2"
         [Playfield 200 100 0 150, Playfield 800 500 130 100]
         [Pos 50 200]
         [Exit (Playfield 70 200 0 50) (Pos 50 200) "middle" 0 "w-resize"]
         (fromList [])

scene3 : Scene
scene3 = Scene
         "bg3"
         [ Playfield 1000 200 200 350
         , { width = 100, height = 140, x = 590, y = 150 }
         , { width = 100, height = 140, x = 873, y = 150 }
         ]
         [ Pos 1150 450 ]
         [ Exit (Playfield 100 250 1100 200) (Pos 1150 450) "middle" 0 "e-resize" ]
         (fromList [ ("panel1", ItemLocation { width = 100, height = 140, x = 294, y = 150 }
                          (Just (Item 100 140 "tile-1" "This tile says one."))
                          (Pos 350 380))
                   , ("panel2", ItemLocation { width = 100, height = 140, x = 590, y = 150 }
                          (Just (Item 100 140 "tile-2" "This tile says two."))
                          (Pos 640 380))
                   , ("panel3", ItemLocation { width = 100, height = 140, x = 873, y = 150 }
                          (Just (Item 100 140 "tile-3" "This tile says three."))
                          (Pos 900 380))
         ])

scenes : List Scene
scenes = [demoScene, scene2, scene3]

sceneDict : Dict String Scene
sceneDict = Dict.fromList [("middle", demoScene), ("west", scene3), ("east", scene2)]

-- Much later we might need ticks always or more of the time...
-- For now I just don't want the extra history in reactor
subscriptions : State -> Sub Msg
subscriptions model = Sub.batch [ presses Key
                                , case model.character.state of
                                      Still -> Sub.none
                                      MovingTo _ _ _ -> diffs Tick
                                ]

type alias State = { character : Character
                   , currentScene : String
                   , scenes : Dict String Scene
                   , debug : Bool
                   , clickData : List Pos -- For random debugging/design purposes...
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
                       , inventory : List Item
                       }

type alias Item = { width : Float
                  , height : Float
                  , img : String
                  , name : String -- Possibly debug-only...
                  }

-- General purpose on-screen item location...
-- Might be re-usable for inventory...
-- For now, any item can be put "anywhere"...
-- Later, some items will be pick-up only, fit specific item types, etc.
type alias ItemLocation = { field : Playfield
                          , contents : Maybe Item
                          , collectPoint : Pos -- Point to walk to before interacting, should be in a field
                          }

type Facing = Left | Right    

-- MovingTo - multi-segment movement plan
type CharState = Still | MovingTo (List Pos) InAnimation Action

type Action = None
            | Leave Exit
            | UseItemLocation String

type alias AnimCycle = List (String, Time)

-- Very simple animation system
-- Or it was supposed to be...
type alias InAnimation = { segments : AnimCycle
                         , current : AnimCycle
                         }

type alias Cursor = String    

type alias Scene = { image : String
                   , playfields : List Playfield
                   , entrance : List Pos
                   , exits : List Exit
                   , itemLocations : Dict String ItemLocation
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
                  , destination : String
                  , destinationSpawn : Int
                  , cursor : String -- hack hack hack
                  }

type Msg = Tick Time
         | FloorClick Int Int
         | ExitClick Int Int
         | StrayClick Int Int -- Debugging...
         | ItemLocationClick String Int Int
         | Key KeyCode

-- Debug stuff, non-essential
addClick : Pos -> State -> State
addClick pos model = { model | clickData = pos :: model.clickData }

clickPos : Int -> Int -> Pos                     
clickPos x y = Pos (toFloat x) (toFloat y)

collectClicks : Msg -> State -> State               
collectClicks msg modelIn =
    case msg of
        ExitClick x y -> addClick (clickPos x y) modelIn
        FloorClick x y -> addClick (clickPos x y) modelIn
        StrayClick x y -> addClick (clickPos x y) modelIn
        _ -> modelIn

debugField : State -> String             
debugField model =
    case model.clickData of
        p2 :: p1 :: _ -> "Pos 1: " ++ toString p1 ++ "; Pos 2: " ++ toString p2 ++ "; Field: " ++
                         toString (Playfield (p2.x - p1.x) (p2.y - p1.y) p1.x p1.y)
        _ -> "No dice"

-- In the near future, split in-game actions from debugging ones and handle separately... 
update : Msg -> State -> ( State, Cmd Msg )
update msg modelIn =
    let model = collectClicks msg modelIn -- Fungible. For debugging.
        s = get model.currentScene model.scenes in
    case s of
        Nothing -> Debug.log "No scene..." (model, Cmd.none)
        Just scene -> (updateWithScene msg model scene, Cmd.none)

-- TODO: Move State out via action and declare victory over abstraction                      
updateWithScene : Msg -> State -> Scene -> State                      
updateWithScene msg model scene = 
    let char = model.character in
    case msg of
        Key p -> if p == toCode 'd'
                  then { model | debug = not model.debug }
                  else if p == toCode 'f'
                       then Debug.log (debugField model) model
                       else model
        Tick delta ->
            let (newChar, action) = walk char delta -- In the future, maybe things can happen other ways... ie ambient timers or animations
                newState = { model | character = newChar }
            in case action of
                   Nothing -> newState
                   Just a -> doAction a newState
        ExitClick x y ->
            case findExit scene.exits (clickPos x y) of
                Nothing -> Debug.log "Bad exit click?" model
                Just exit ->
                    let ps = walkOneX char.pos scene.playfields exit.position
                        newState = case ps of
                                       Nothing -> Debug.log "Failed exit click..." Still
                                       Just ps -> MovingTo ps (InAnimation char.walkCycle char.walkCycle) (Leave exit)
                    in { model | character = { char | state = newState } }
        FloorClick x y ->
            let newState = 
                    case walkOneX char.pos scene.playfields (clickPos x y)
                    of
                        Nothing -> Still
                        Just ps -> MovingTo ps (InAnimation char.walkCycle char.walkCycle) None
            in { model | character = { char | state = newState } }
        ItemLocationClick k x y ->
            get k scene.itemLocations |> maybe model
                (\itemLoc ->
                     let newState = 
                     case walkOneX char.pos scene.playfields itemLoc.collectPoint
                     of
                         Nothing -> Debug.log "Failure routing to item location" Still
                         Just ps -> MovingTo ps (InAnimation char.walkCycle char.walkCycle)
                         <| UseItemLocation k
                     in { model | character = { char | state = newState } })
            
        StrayClick _ _ -> model

maybe : b -> (a -> b) -> Maybe a -> b
maybe def f m = withDefault def (Maybe.map f m)        

doAction : Action -> State -> State
doAction action model =
    case action of
        None -> model
        UseItemLocation itemKey ->
            -- This isn't much or any better than chaining andThen >_<
            get model.currentScene model.scenes |> maybe model
                (\scene -> get itemKey scene.itemLocations |> maybe model
                     (\itemLoc -> 
                          case itemLoc.contents of
                              Nothing -> Debug.log "TODO: Put something in..." model
                              Just i -> grabItemFrom i itemKey itemLoc model))
        Leave exit ->
            let s = get exit.destination model.scenes in
            case s of
                Nothing -> Debug.log "No scene (doAction)..." model
                Just scene ->
                    let spawn = List.head <| List.drop exit.destinationSpawn scene.entrance
                        char = model.character 
                    in case spawn of 
                           Just spawn ->
                               { model | currentScene = exit.destination
                               , character = { char | pos = spawn } } -- TODO: Position character...
                           _ -> Debug.log "Missing scene" model

-- This is excruciating
-- Afterwards, the item location is emptied in its slot, and the character has the item in head of inventory
grabItemFrom : Item -> String -> ItemLocation -> State -> State                                
grabItemFrom item itemKey itemLoc model =
    let char = model.character in
    { model | scenes = Dict.update model.currentScene (Maybe.map <| emptySceneItemLocation itemKey) model.scenes
    , character = { char | inventory = item :: char.inventory }
    }

emptySceneItemLocation : String -> Scene -> Scene
emptySceneItemLocation itemKey scene =
    { scene | itemLocations = Dict.update itemKey (Maybe.map emptyItemLocation) scene.itemLocations }

emptyItemLocation : ItemLocation -> ItemLocation
emptyItemLocation itemLoc = { itemLoc | contents = Nothing }                    
    

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
  let s = get model.currentScene model.scenes in
  case s of
      Nothing -> text "No scene, very bad error."
      Just scene -> renderScene scene model.character model.debug

renderScene : Scene -> Character -> Bool -> Html Msg                    
renderScene scene char debug =
    div [ style [ ("background-image", "url(img/" ++ scene.image ++ ".png)")
                , ("width", "1400px")
                , ("height", "700px")
                ]
        , onWithOptions "click" selfish (offsetPosition StrayClick)
        ]
      [ div [] (if debug then
                    (List.map (viewDebugField "blue") scene.playfields)
                    ++ (List.map (viewDebugField "green" << .field) scene.exits)
                    ++ (List.map (viewDebugPos "e" << .position) scene.exits)
                    ++ (List.map (viewDebugPos "i" << .collectPoint) (values scene.itemLocations))
                    ++ (List.map (viewDebugField "yellow" << .field) (values scene.itemLocations)) else [])
      , div [] (List.map viewItemLocation (values scene.itemLocations))
      , viewChar char
      , div [] (if debug then [viewDebugPos "C" char.pos] else [])
      -- These need to be "on top". This is not really a good solution.
      , div [] (List.map (clickField identity (always FloorClick) (always "crosshair")) scene.playfields)
      , div [] (List.map (clickField (.field) (always ExitClick) (.cursor)) scene.exits)
      , div [] (List.map (clickField (.field << second) (ItemLocationClick << first) (always "move")) (toList scene.itemLocations))
      , viewInventory char.inventory
      ]

viewInventory : List Item -> Html Msg
viewInventory items =
    div [ style [ ("position", "absolute")
                , ("bottom", "20px")
                , ("left", "10px")
                ]
        ]
        (List.map viewInvItem items)

viewInvItem : Item -> Html Msg
viewInvItem item =
    img [ src <| "img/" ++ item.img ++ ".png" ] []

viewItemLocation : ItemLocation -> Html Msg
viewItemLocation itemLoc =
    case itemLoc.contents of
        Nothing -> text ""
        Just item ->
            div [ style [ ("height", toString item.height ++ "px")
                         , ("width", toString item.width ++ "px")
                         , ("position", "absolute")
                         , ("top", toString itemLoc.field.y ++ "px") -- fudge fudge
                         , ("left", toString itemLoc.field.x ++ "px")
                         ] ] [ img [src ("img/" ++ item.img ++ ".png"), style [ ("width", toString item.width ++ "px" ) ] ] [] ]
            
selfish : Options
selfish = Options True True

-- https://github.com/fredcy/elm-svg-mouse-offset/blob/master/Main.elm    
offsetPosition : (Int -> Int -> Msg) -> Json.Decoder Msg
offsetPosition msg = Json.map2 msg (Json.field "pageX" Json.int) (Json.field "pageY" Json.int)

-- A lot of overlap with viewDebugField...                     
clickField : (a -> Playfield) -> (a -> Int -> Int -> Msg) -> (a -> Cursor) -> a -> Html Msg
clickField f m cur e = div [ style [ ("height", toString (f e).height ++ "px")
                                   , ("width", toString (f e).width ++ "px")
                                   , ("position", "absolute")
                                   , ("top", toString (f e).y ++ "px") -- fudge fudge
                                   , ("left", toString (f e).x ++ "px")
                                   , ("cursor", cur e)
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
