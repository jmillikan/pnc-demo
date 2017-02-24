module DebugEdit exposing (..)

import Dict
import List
import Html exposing (Html, div, text, input, button)
import Result exposing (andThen)
import Char exposing (fromCode)
import Html.Attributes exposing (style, placeholder)
import Html.Events exposing (onInput, onClick)

import GameState exposing (..)
import Expect exposing (..)

-- DebugEdit: A random assortment of debugging/editing stuff
-- Some of it is a subcomponent of debugView/debugUpdate/DebugCmd/DebugMsg

type DebugCmd = AddingScene String
              | ChangingScene String
              | AddingExit String String String

type DebugMsg = ChangeName String
              | ChangeSpawn String
              | ChangeEname String
              | Finish
              | Cancel

debugView : DebugCmd -> Html DebugMsg
debugView cmd =
    case cmd of
        AddingScene s -> div [ style [ ("position", "absolute"), ("top", "100px"), ("left", "100px") ] ]
                         [ input [ placeholder "Scene key", onInput ChangeName ] []
                         , button [ onClick Finish ] [ text "Add Scene" ]
                         , button [ onClick Cancel ] [ text "Cancel" ]
                         ]
        ChangingScene s -> div [ style [ ("position", "absolute"), ("top", "100px"), ("left", "100px") ] ]
                         [ input [ placeholder "Scene key", onInput ChangeName ] []
                         , button [ onClick Finish ] [ text "Go Scene" ]
                         , button [ onClick Cancel ] [ text "Cancel" ]
                         ]
        AddingExit s i e -> div [ style [ ("position", "absolute"), ("top", "100px"), ("left", "100px") ] ]
                         [ input [ placeholder "Scene key", onInput ChangeName ] []
                         , input [ placeholder "Spawn idx", onInput ChangeSpawn ] []
                         , input [ placeholder "Exit name", onInput ChangeEname ] []
                         , button [ onClick Finish ] [ text "Go Scene" ]
                         , button [ onClick Cancel ] [ text "Cancel" ]
                         ]

-- Using Result as Either...
-- Keep updating until we're done and ready to change the world
debugUpdate : DebugCmd -> DebugMsg -> World -> Result DebugCmd World
debugUpdate cmd msg w =
    let (continue, done) = (Err, Ok) in
    case (cmd, msg) of
        (AddingScene _, ChangeName s) -> continue <| AddingScene s
        (AddingScene s, Finish) -> done <| addScene s w
        (ChangingScene _, ChangeName s) -> continue <| ChangingScene s
        (ChangingScene s, Finish) -> done <| goScene s w
        (AddingExit _ i e, ChangeName s) -> continue <| AddingExit s i e
        (AddingExit s _ e, ChangeSpawn i) -> continue <| AddingExit s i e
        (AddingExit s i _, ChangeEname e) -> continue <| AddingExit s i e
        (AddingExit s i e, Finish) -> done <| addExit e s i w
        (_, Cancel) -> done <| w
        _ -> continue cmd

addExit : String -> String -> String -> World -> World
addExit eName scene idx w =
    case (String.toInt idx, w.clickData) of
        (Ok i, p3 :: p2 :: p1 :: _) ->
            let e = 
            expectUpdate w.currentScene w.scenes (\cs -> Ok <| { cs | usables = Dict.insert eName (newExit scene i p1 p2 p3) cs.usables })
                |> Result.map (\scenes -> { w | scenes = scenes }) in
            case e of
                Ok newW -> newW
                Err _ -> Debug.log "Rand problem adding exit" w
                        
        _ -> Debug.log "Rando problem adding exit" w

newExit : String -> Int -> Pos -> Pos -> Pos -> Usable
newExit scene spawn p1 p2 useP =
    Usable (newField p1 p2) (Leave scene spawn) (Just <| useP) Nothing "pointer" True

addScene : String -> World -> World
addScene s w = { w | scenes = Dict.insert s (newScene s) w.scenes }

goScene : String -> World -> World
goScene s w = { w | currentScene = s }          

-- New scene with a non-existent background image               
newScene : String -> Scene
newScene s = { image = s, playfields = [], entrance = [], itemLocations = Dict.fromList [], usables = Dict.fromList [] }

-- This doesn't need to change GameState yet, esp. since World includes Character movement and debug click data...
doDebugKeys : Char.KeyCode -> World -> Result String World
doDebugKeys p world =
    case fromCode p of
        'h' -> Debug.log "Instructions: Click - cons point to stack\n?: View point stack\nf: Add field from stack\nw/W: Print world json/expr\nb: Load blank world\ns: Add scene\nn: Add spawn" () |> always (Ok world)
        '?' -> Debug.log "Point stack" (toString (List.take 5 world.clickData) ++ "...") |> always (Ok world)
        'f' -> dbgAddField world
        'n' -> dbgAddSpawn world
        'w' -> Debug.log "World JSON" (worldE world) |> always (Ok world)
        'W' -> Debug.log "World expr" world |> always (Ok world)
        'b' -> Ok dbgBlankWorld
        -- 's' -> ... I've hit a wall here, need to jump back into Main to figure out how to rig the subcomponent...
        _ -> Err "Unknown debug key"
    
debugField : World -> String             
debugField model =
    case model.clickData of
        p2 :: p1 :: _ -> "Pos 1: " ++ toString p1 ++ "; Pos 2: " ++ toString p2 ++ "; Field: " ++
                         toString (newField p1 p2)
        _ -> "No dice"

dbgAddField : World -> Result String World
dbgAddField w =
    case w.clickData of
        p2 :: p1 :: restClicks ->
            expectUpdate w.currentScene w.scenes (\cs -> Ok <| { cs | playfields = newField p1 p2 :: cs.playfields })
                |> Result.map (\scenes -> { w | scenes = scenes })
        _ -> Err "Not enough click data"

dbgAddSpawn : World -> Result String World
dbgAddSpawn w =
    case w.clickData of
        p1 :: _ -> expectUpdate w.currentScene w.scenes
                   (\cs -> Ok <| { cs | entrance = p1 :: cs.entrance })
                |> Result.map (\scenes -> { w | scenes = scenes })
        _ -> Err "Not enough click data"

dbgBlankWorld : World
dbgBlankWorld = { character = { width = 120, height = 180, pos = { x = 200, y = 300 }, state = Still, speed = 0.2, facing = Right, walkCycle = [("1",500),("2",500)], inventory = [] }, currentScene = "blank", scenes = Dict.fromList [("blank",newScene "blank")], debug = True, clickData = [{ x = 391, y = 157 },{ x = 442, y = 211 },{ x = 500, y = 528 }] }                

newField : Pos -> Pos -> Playfield
newField p1 p2 = Playfield (p2.x - p1.x) (p2.y - p1.y) p1.x p1.y           

viewDebugPos : String -> Pos -> Html a
viewDebugPos c p = div
                  [ style [ ("position", "absolute")
                          , ("top", toString p.y ++ "px") -- fudge fudge
                          , ("left", toString p.x ++ "px")
                          , ("border", "solid red")
                          , ("border-width", "2px 0px 0px 2px")
                          ] ]
                  [ text c ]
             
viewDebugField : String -> Playfield -> Html a
viewDebugField color f = div [ style [ ("background-color", color)
                               , ("height", toString f.height ++ "px")
                               , ("width", toString f.width ++ "px")
                               , ("position", "absolute")
                               , ("top", toString f.y ++ "px")
                               , ("left", toString f.x ++ "px")
                               ] ] [ text "field" ]
