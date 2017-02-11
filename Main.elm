import Html exposing (Html, button, div, img, text)
import Html.Events exposing (onClick, onWithOptions, Options)
import Html.Attributes exposing (style, src)
import Time exposing (Time, millisecond)
import AnimationFrame exposing (diffs)
import Json.Decode as Json
import Keyboard exposing (KeyCode, presses)
import Char exposing (toCode)
import Maybe exposing (withDefault)
import Dict exposing (Dict, fromList, toList, get, values)
import Tuple exposing (first, second)
import Result exposing (Result, andThen)

import GameState exposing (..)

import DemoLevel exposing (demoState)

main : Program Never State Msg
main = Html.program { init = (demoState, Cmd.none), view = view, update = update, subscriptions = subscriptions }

-- The bulk of the code divides up about five/six ways:
-- Types, mostly game/planning state
-- Demo data (three scenes)
-- update & debug
-- updateWithScene (except walk) & primitives
-- walk & primitives
-- doAction & primitives

-- In-progress movement from Maybe and error-free errors to Result String x...

-- Much later we might need ticks always or more of the time...
-- For now I just don't want the extra history in reactor
subscriptions : State -> Sub Msg
subscriptions model = Sub.batch [ presses Key
                                , case model.character.state of
                                      Still -> Sub.none
                                      MovingTo _ _ _ -> diffs Tick
                                ]

type Msg = Tick Time
         | FloorClick Int Int
         | ExitClick Int Int
         | StrayClick Int Int -- Debugging...
         | ItemLocationClick String Int Int
         | Key KeyCode

-- Non-essential stuff for debugging and fudging level elements
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

-- Start clearing out the Maybe thicket...             
expectIn : Dict comparable v -> comparable -> Result String v
expectIn dict k = Result.fromMaybe ("Missing expected value for key " ++ toString k)
                  <| get k dict

-- In the near future, split in-game actions from debugging/menu/whatever and handle separately... 
update : Msg -> State -> ( State, Cmd Msg )
update msg modelIn =
    let model = collectClicks msg modelIn in -- Fungible. For debugging.
    let res =
    case msg of
        Key p -> if p == toCode 'd'
                  then Ok { model | debug = not model.debug }
                  else if p == toCode 'f'
                       then Ok <| Debug.log (debugField model) model
                       else Err "Unknown keystroke"
        _ -> expectIn model.scenes model.currentScene
                |> andThen (\scene -> updateWithScene msg model.character scene) 
                |> Result.map (\(newChar, act) -> ({ model | character = newChar }, act))
                |> andThen (\(newModel, act) -> maybe (Ok newModel) (doAction newModel) act)
                   
    in case res of
           Err err -> Debug.log ("Error updating: " ++ err) (model, Cmd.none)
           Ok m -> (m, Cmd.none)

-- So far there are really only three things that happen here, either:
-- a) Character get a plan to do something (walk and/or take an action)
-- b) Character takes an action (that may effect just about anything)
-- c) Character walks a little bit
-- So this is limited to the character and scene for now, and actions happen outside.
-- In fact Action could subsume character movement and this would get a lot cleaner.
updateWithScene : Msg -> Character -> Scene -> Result String (Character, Maybe Action)
updateWithScene msg char scene =
    case msg of
        Key _ -> Err "Key shoudldn't be here (TODO)"
        Tick delta -> Ok <| walk char delta
        ExitClick x y -> -- In theory this is a bit easier than the alternatives
            findExit scene.exits (clickPos x y)
                |> andThen (\exit -> walkOneX char.pos scene.playfields exit.position
                                -- \ps -> ... could move outside.
                           |> Result.map (\ps -> let newState = MovingTo ps (InAnimation char.walkCycle char.walkCycle) (Leave exit)
                                                 in ({ char | state = newState }, Nothing)))
        FloorClick x y ->
            walkOneX char.pos scene.playfields (clickPos x y)
                |> Result.map (\ps -> let newState = MovingTo ps (InAnimation char.walkCycle char.walkCycle) None
                                      in ({ char | state = newState }, Nothing))
        ItemLocationClick k x y ->
            expectIn scene.itemLocations k
                |> andThen (\itemLoc -> walkOneX char.pos scene.playfields itemLoc.collectPoint)
                |> Result.map (\ps -> let newState = MovingTo ps (InAnimation char.walkCycle char.walkCycle)
                                                     <| UseItemLocation k
                                      in ({ char | state = newState }, Nothing))
        StrayClick _ _ -> Err "Can't do anything with a stray click. (This is okay.)"

maybe : b -> (a -> b) -> Maybe a -> b
maybe def f m = withDefault def (Maybe.map f m)        

doAction : State -> Action -> Result String State
doAction model action =
    case action of
        None -> Ok model
        UseItemLocation itemKey ->
            expectIn model.scenes model.currentScene
                |> andThen (\scene -> expectIn scene.itemLocations itemKey)
                |> Result.map (\itemLoc -> itemLoc.contents |> maybe
                                   (putItemIn itemKey model)
                                   (\i -> grabItemFrom i itemKey itemLoc model))
        Leave exit ->
            expectIn model.scenes exit.destination
                |> andThen (\scene -> Result.fromMaybe "No spawn found..." -- TODO: Helper, or change to dict
                                      <| List.head <| List.drop exit.destinationSpawn scene.entrance)
                |> Result.map (\spawn -> let char = model.character
                                         in { model | currentScene = exit.destination
                                            , character = { char | pos = spawn } })

-- This is excruciating
-- Afterwards, the item location is emptied in its slot, and the character has the item in head of inventory
grabItemFrom : Item -> String -> ItemLocation -> State -> State                                
grabItemFrom item itemKey itemLoc model =
    let char = model.character in
    { model | scenes = Dict.update model.currentScene (Maybe.map <| emptySceneItemLocation itemKey) model.scenes
    , character = { char | inventory = item :: char.inventory }
    }

-- Character loses left-most item (for now)
-- leftmost item goes into itemLocation
putItemIn : String -> State -> State
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

-- expectAt and expectIn are different ways round. It's bad.
-- Should just toss the lists and use Dict, but I have too much brain load right now                           
expectAt : (a -> Bool) -> List a -> Result String a
expectAt f l = Result.fromMaybe ("Expected to find item in " ++ toString l)
                <| List.head <| List.filter f l

findExit : List Exit -> Pos -> Result String Exit
findExit exits pos = expectAt (pointInPlayfield pos << .field) exits

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

-- Render a background image and character
-- Optionally render some debug geometry...
-- For now the view is fixed in scale... Someday, scale it along with click events to accomodate
-- the poor, miserable wretches who don't have my specific laptop
view : State -> Html Msg
view model =
  let s = get model.currentScene model.scenes in
  case s of
      Nothing -> text "No scene, very bad error."
      Just scene ->
          div []
              [ renderScene scene model.character model.debug
              , viewInventory model.character.inventory
              ]

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
