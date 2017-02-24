import AnimationFrame exposing (diffs)
import Char exposing (toCode)
import Dict exposing (Dict, fromList, toList, get, values)
import Html exposing (Html, button, div, img, text)
import Html.Attributes exposing (style, src)
import Html.Events exposing (onClick, onWithOptions, Options)
import Json.Decode as Json
import Keyboard exposing (KeyCode, presses)
import Maybe exposing (withDefault)
import Result exposing (Result, andThen)
import Time exposing (Time, millisecond)
import Tuple exposing (first, second)
import Http

import GameState exposing (..)
import DebugEdit exposing (..)
import Expect exposing (..)

-- Main: The majority of the runtime bits of the game, except for World and contents which are in GameState

main : Program Never GameState Msg
main = Html.program { init = (Menu, Cmd.none), view = view, update = update, subscriptions = subscriptions }

type GameState = Menu
               | Interact World -- We're playing
               | Animate GameAnimation World
               | DebugGame DebugCmd World

subscriptions : GameState -> Sub Msg
subscriptions model = Sub.batch [ presses Key
                                , diffs Tick
                                ]

type Msg = Tick Time
    -- The *Click stuff will get compacted when objects are generalized
         | FloorClick Pos
         | StrayClick Pos -- Debugging...
         | ItemLocationClick String Pos
         | UsableClick String Pos
         | Key KeyCode
         | LoadWorld (Result Http.Error World)
         | DebugMsg DebugMsg

-- Non-essential stuff for debugging and fudging level elements
addClick : Pos -> World -> World
addClick pos model = { model | clickData = pos :: model.clickData }

clickPos : Int -> Int -> Pos                     
clickPos x y = Pos (toFloat x) (toFloat y)

collectClicks : Msg -> World -> World               
collectClicks msg modelIn =
    case msg of
        FloorClick pos -> addClick pos modelIn
        StrayClick pos -> addClick pos modelIn
        UsableClick _ pos -> addClick pos modelIn
        _ -> modelIn

update : Msg -> GameState -> ( GameState, Cmd Msg )
update msg state =
    case state of
        Menu -> case msg of -- Click anywhere to start game...
                StrayClick _ -> (Menu, Http.send LoadWorld <| Http.get "world.json" world)
                LoadWorld w -> case w of
                                   Ok w -> (Interact w, Cmd.none)
                                   Err e -> Debug.log "Error loading world" e |> always (Menu, Cmd.none)
                _ -> (state, Cmd.none)
        Interact world -> (updateWorld msg world, Cmd.none)
        -- Stay in the debugging/editing command until it is done, passing back a new world
        DebugGame debugCmd w ->
            case msg of
                DebugMsg debugMsg ->
                    case debugUpdate debugCmd debugMsg w of
                        Ok newWorld -> (Interact newWorld, Cmd.none)
                        Err keepDebugging -> (DebugGame keepDebugging w, Cmd.none)
                _ -> (DebugGame debugCmd w, Cmd.none)
        -- During cut scenes, all interactions get lost
        Animate cutScene world ->
            case msg of
                Tick delta ->
                    case runCutScene delta cutScene world of
                        Ok newState -> (newState, Cmd.none)
                        -- Errors in animations will probably not resolve at runtime.
                        Err err -> Debug.log "Error" err |> always (state, Cmd.none)
                       
                _ -> (state, Cmd.none)                     

-- Due to not having nested Msg yet, this all is wrong-shaped                     
updateWorld : Msg -> World -> GameState
updateWorld msg modelIn =
    let model = collectClicks msg modelIn in -- Fungible. For debugging.
    let res =
    case msg of
        Key p -> if p == toCode 'd'
                  then Ok <| Interact { model | debug = not model.debug }
                  else -- Disable keys while not debugging for minimum surprise...
                      if not model.debug
                      then Err "Not in debug mode, no keys accepted"
                      else if p == toCode 's'
                          then Ok <| DebugGame (AddingScene "") model
                           else if p == toCode 'g'
                               then Ok <| DebugGame (ChangingScene "") model
                                else if p == toCode 'e'
                                     then Ok <| DebugGame (AddingExit "" "" "") model
                                     else doDebugKeys p model |> Result.map Interact
                                       
        _ -> expectIn model.scenes model.currentScene
                |> andThen (\scene -> updateChar msg model.character scene) 
                |> Result.map (\(newChar, act) -> ({ model | character = newChar }, act))
                |> Result.map (\(newModel, act) -> maybe
                                (Interact newModel)
                                -- Absorb failures in doAction so they don't loop...
                                (doAction newModel >>
                                     Result.mapError (Debug.log "Error in doAction") >> 
                                     Result.withDefault (Interact newModel) ) act)
    in case res of
           Err err -> Debug.log ("Error updating: " ++ err) (Interact model)
           Ok m -> m

-- So far there are really only three things that happen here, either:
-- a) Character get a plan to do something (walk and/or take an action)
-- b) Character takes an action (that may effect just about anything)
-- c) Character walks a little bit
-- So this is limited to the character and scene for now, and actions happen outside.
-- In fact Action could subsume character movement and this would get a lot cleaner.
updateChar : Msg -> Character -> Scene -> Result String (Character, Maybe Action)
updateChar msg char scene =
    case msg of
        LoadWorld _ -> Err "Can only load world from menu for now"
        Key _ -> Err "Key shoudldn't be here (TODO)"
        Tick delta -> Ok <| walk char delta
        FloorClick pos -> planWalk char scene None pos
        ItemLocationClick k _ ->
            expectIn scene.itemLocations k
                |> andThen (planWalk char scene (UseItemLocation k) << .collectPoint)
        UsableClick k _ -> expectIn scene.usables k                           
                        |> andThen (\usable ->
                                        case usable.usePoint of
                                            Just p -> planWalk char scene usable.event p
                                            Nothing -> Ok <| (char, Just usable.event))
        StrayClick _ -> Err "Can't do anything with a stray click. (This is okay.)"
        DebugMsg _ -> Err "Can't handle debugger messages in interact"

planWalk : Character -> Scene -> Action -> Pos -> Result String (Character, Maybe a)
planWalk char scene action pos =
    walkOneX char.pos scene.playfields pos
        |> Result.map (\ps ->
                           let newState = MovingTo ps (InAnimation char.walkCycle char.walkCycle) action
                           in ({ char | state = newState }, Nothing))


-- MAYBE AND RESULT UTILS           
maybe : b -> (a -> b) -> Maybe a -> b
maybe def f m = withDefault def (Maybe.map f m)

doAction : World -> Action -> Result String GameState
doAction model action =
    case action of
        None -> Ok <| Interact model
        ReturnToMenu -> Ok <| Menu
        GoScene s -> Ok <| Interact { model | currentScene = s } -- Overlaps Leave
        UseItemLocation itemKey ->
            expectIn model.scenes model.currentScene
                |> andThen (\scene -> expectIn scene.itemLocations itemKey)
                |> Result.map (\itemLoc -> itemLoc.contents |> maybe
                                   (putItemIn itemKey model) -- These two should be Result but I haven't gotten to it
                                   (\i -> grabItemFrom i itemKey itemLoc model))
                |> Result.map Interact
        AnimateUsable key time anim ->
            getWorldUsableImage model key
                |> Result.map (\oldImage ->
                                   Animate (AnimationUsable key time (InAnimation anim anim) oldImage None) model)
        ContentsCheck itemChecks successAction failAction ->
            checkContents itemChecks model
                |> andThen (\itemsMatch ->
                                if itemsMatch 
                                then doAction model successAction
                                else doAction model failAction)
        Sequence action1 action2 ->
            doAction model action1
                |> andThen (\res -> case res of
                                        Interact world1 -> doAction world1 action2
                                        _ -> Err "Action 1 tried to leave interact. Halting action 2.")
        ActivateUsable sceneKey usableKey ->
            Result.map Interact <| activate sceneKey usableKey model
        Leave dest spawnIndex ->
            expectIn model.scenes dest
                |> andThen (\scene -> Result.fromMaybe "No spawn found..." -- TODO: Helper, or change to dict
                                      <| List.head <| List.drop spawnIndex scene.entrance)
                |> Result.map (\spawn -> let char = model.character
                                         in Interact { model | currentScene = dest
                                                     , character = { char | pos = spawn } })

checkContents : List (String, String, Maybe String) -> World -> Result String Bool
checkContents itemChecks world =
    case itemChecks of
        [] -> Ok True
        (sceneKey, locKey, checkFor) :: rest ->
            expectIn world.scenes sceneKey
                |> andThen (\scene -> expectIn scene.itemLocations locKey)
                |> andThen (\loc -> case (loc.contents, checkFor)  of
                                        (Just foundItem, Just checkItem) ->
                                            if foundItem.name == checkItem
                                            then checkContents rest world
                                            else Ok False
                                        (Nothing, Nothing) -> checkContents rest world
                                        _ -> Ok False)
                                        
activate : String -> String -> World -> Result String World
activate sceneKey usableKey world =
    expectUpdate sceneKey world.scenes
        (\scene -> expectUpdate usableKey scene.usables
             (\usable -> Ok <| { usable | active = True })
             |> Result.map (\usables -> { scene | usables = usables }))
        |> Result.map (\scenes -> { world | scenes = scenes })

getWorldUsableImage : World -> String -> Result String (Maybe String)
getWorldUsableImage world key =
    expectIn world.scenes world.currentScene
        |> andThen (\scene -> expectIn scene.usables key)
        |> Result.map .img
        
-- Afterwards, the item location is emptied in its slot, and the character has the item in head of inventory
grabItemFrom : Item -> String -> ItemLocation -> World -> World                                
grabItemFrom item itemKey itemLoc model =
    let char = model.character in
    { model | scenes = Dict.update model.currentScene (Maybe.map <| emptySceneItemLocation itemKey) model.scenes
    , character = { char | inventory = item :: char.inventory }
    }

-- Character loses left-most item (for now)
-- leftmost item goes into itemLocation
putItemIn : String -> World -> World
putItemIn itemKey s =
    let char = s.character in
    case char.inventory of
        [] -> s
        item :: newInv -> 
    { s | scenes = Dict.update s.currentScene (Maybe.map <| fillSceneItemLocation itemKey item) s.scenes
    , character = { char | inventory = newInv }
    }

fillSceneItemLocation : String -> Item -> Scene -> Scene
fillSceneItemLocation itemKey item scene =
    { scene | itemLocations = Dict.update itemKey (Maybe.map <| fillItemLocation item) scene.itemLocations }
    
emptySceneItemLocation : String -> Scene -> Scene
emptySceneItemLocation itemKey scene =
    { scene | itemLocations = Dict.update itemKey (Maybe.map emptyItemLocation) scene.itemLocations }

emptyItemLocation : ItemLocation -> ItemLocation
emptyItemLocation itemLoc = { itemLoc | contents = Nothing }                    

fillItemLocation : Item -> ItemLocation -> ItemLocation
fillItemLocation item itemLoc = { itemLoc | contents = Just item }                    

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
walk : Character -> Time -> (Character, Maybe Action)
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
    case a.current of
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
walkOneX : Pos -> List Playfield -> Pos -> Result String (List Pos)
walkOneX sourceP playfields p =
    Result.map2 (,) (findPlayfield playfields sourceP) (findPlayfield playfields p)
        |> andThen (\(src, dest) ->
                        if src == dest
                        then Ok [p]
                        else middleOfX src dest |> Result.map (\firstX -> [firstX, p]))
                     
findPlayfield : List Playfield -> Pos -> Result String Playfield
findPlayfield fields pos = Result.fromMaybe "Can't find pos in playfield"
                           <| List.head <| List.filter (pointInPlayfield pos) fields

pointInPlayfield : Pos -> Playfield -> Bool
pointInPlayfield p field =
    p.x >= field.x
        && p.x <= field.x + field.width
        && p.y >= field.y
        && p.y <= field.y + field.height

-- Middle of intersection of two fields, or Nothing if not intersecting
middleOfX : Playfield -> Playfield -> Result String Pos
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
           then Ok <| Pos ((l + r) / 2) ((t + b) / 2)
           else Err "No intersection between fields"

runCutScene : Time -> GameAnimation -> World -> Result String GameState
runCutScene delta ev world =
    case ev of
        AnimationUsable key timeLeft animation doneImage nextEvent ->
            if timeLeft <= delta
            then setWorldUsableImage world key doneImage
                           |> andThen (\world -> doAction world nextEvent) 
            else -- Ignore animation for now
                let newAnim = advanceAnim animation delta 
                in getAnimImage newAnim
                    |> andThen (\img -> setWorldUsableImage world key (Just img))
                    |> Result.map (\newWorld -> Animate (AnimationUsable key (timeLeft - delta) newAnim doneImage nextEvent) newWorld)

getAnimImage : InAnimation -> Result String String
getAnimImage anim =
    Result.fromMaybe "Animation is empty or something"
        <| Maybe.map first <| List.head anim.current

setWorldUsableImage : World -> String -> Maybe String -> Result String World
setWorldUsableImage world key img =
    expectUpdate world.currentScene world.scenes (setSceneUsableImage key img)
        |> Result.map (\newScenes -> { world | scenes = newScenes })

setSceneUsableImage : String -> Maybe String -> Scene -> Result String Scene
setSceneUsableImage key img scene =
    expectUpdate key scene.usables (\usable -> Ok <| { usable | img = img })
        |> Result.map (\newUsables -> { scene | usables = newUsables })

view : GameState -> Html Msg
view state =
    case state of
        Menu -> viewMenu
        Interact world -> viewWorld world
        Animate cutScene world -> viewWorld world
        DebugGame debugCmd world ->
            div [] [ viewWorld world
                   , Html.map DebugMsg (debugView debugCmd)
                   ]

viewMenu : Html Msg
viewMenu = div [ style [ ("background-image", "url(img/menu.png)")
                       , ("background-repeat", "no-repeat")
                       , ("width", "1400px")
                       , ("height", "700px")
                       ]
               , onWithOptions "click" selfish (offsetPosition StrayClick)
               ] [ ]

-- Render a background image and character
-- Optionally render some debug geometry...
-- For now the view is fixed in scale... Someday, scale it along with click events to accomodate
-- the poor, miserable wretches who don't have my specific laptop
viewWorld : World -> Html Msg
viewWorld model =
  let s = get model.currentScene model.scenes in
  case s of
      Nothing -> text "No scene, very bad error."
      Just scene ->
          div []
              [ renderScene scene model.character model.debug
              , viewInventory model.character.inventory
              ]

-- TODO: z-ordering affecting at least objects (Usables, ItemLocations) and character              

-- Draw a bunch of things. Stacking order is important.
-- SVG might be better. TODO.
-- I'm not even worrying about how images are scaled or match object bounds yet
renderScene : Scene -> Character -> Bool -> Html Msg                    
renderScene scene char debug =
    let usables = Dict.filter (always .active) scene.usables
    in
    div [ style [ ("background-image", "url(img/" ++ scene.image ++ ".png)")
                , ("width", "1400px")
                , ("height", "700px")
                ]
        , onWithOptions "click" selfish (offsetPosition StrayClick)
        ]
      [ div [] (if debug then
                    (List.map (viewDebugField "blue") scene.playfields)
                    ++ (List.map (viewDebugPos "i" << .collectPoint) (values scene.itemLocations))
                    ++ (List.map (viewDebugPos "u") (List.filterMap .usePoint <| values usables))
                    ++ (List.map (viewDebugPos "e") scene.entrance)
                    ++ (List.map (viewDebugField "yellow" << .field) (values scene.itemLocations))
                    ++ (List.map (viewDebugField "orange" << .field) (values usables)) else [])
      , div [] (List.map viewItemLocation (values scene.itemLocations))
      , div [] (List.map viewUsable (values usables))
      , viewChar char
      , div [] (if debug then [viewDebugPos "C" char.pos] else [])
      -- These need to be "on top". This is not really a good solution.
      , div [] (List.map (clickField identity (always FloorClick) (always "crosshair")) scene.playfields)
      , div [] (List.map (clickField (.field << second) (ItemLocationClick << first) (always "move")) (toList scene.itemLocations))
      , div [] (List.map (clickField (.field << second) (UsableClick << first) (.cursor << second))
                    (toList usables))
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
viewInvItem item = img [ src <| "img/" ++ item.img ++ ".png" ] []

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

viewUsable : Usable -> Html Msg
viewUsable u =
    case u.img of
        Nothing -> div [ ] [ ]
        Just tag -> div [ style [ ("height", toString u.field.height ++ "px")
                                , ("width", toString u.field.width ++ "px")
                                , ("position", "absolute")
                                , ("top", toString u.field.y ++ "px") -- fudge fudge
                                , ("left", toString u.field.x ++ "px")
                                ] ] [ img [src ("img/" ++ tag ++ ".png"), style [ ("width", toString u.field.width ++ "px" ) ] ] [] ]
            
selfish : Options
selfish = Options True True

-- https://github.com/fredcy/elm-svg-mouse-offset/blob/master/Main.elm    
offsetPosition : (Pos -> Msg) -> Json.Decoder Msg
offsetPosition msg = Json.map2 (((<<) << (<<)) msg clickPos) (Json.field "pageX" Json.int) (Json.field "pageY" Json.int)

-- A lot of overlap with viewDebugField...                     
clickField : (a -> Playfield) -> (a -> Pos -> Msg) -> (a -> Cursor) -> a -> Html Msg
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
             MovingTo _ a _ -> List.head a.current |> maybe "1" first
                               
face : Character -> String
face c = case c.facing of
             Left -> "left"
             Right -> "right"

