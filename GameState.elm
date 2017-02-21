module GameState exposing(..)

import Dict exposing (Dict, toList)
import Time exposing (Time)

import Json.Decode as D exposing (..)
import Json.Encode as E

-- GameState: State of game including World, plus World encoder/decoder for loading/saving

-- Quick and dirty object decoders with explicit type and predictable encoders
-- There's similar stuff in Json.Helpers that I should probably be using

withType : String -> Decoder a -> Decoder a
withType t d = (field "type" string)
           |> andThen (\tag -> if tag == t then d else fail "Wrong type")

branch0 : a -> String -> Decoder a
branch0 cons name = withType name <| succeed cons -- Consistency...
branch1 : (a -> value) -> String -> ( String, Decoder a ) -> Decoder value
branch1 cons name (name1, t1)  = withType name <| map cons (field name1 t1)
branch2 : (a -> a1 -> value) -> String -> ( String, Decoder a ) -> ( String, Decoder a1 ) -> Decoder value
branch2 cons name (name1, t1) (name2, t2)  = withType name <| map2 cons (field name1 t1) (field name2 t2)
branch3 : (a -> a1 -> a2 -> value) -> String -> ( String, Decoder a ) -> ( String, Decoder a1 ) -> ( String, Decoder a2 ) -> Decoder value
branch3 cons name (name1, t1) (name2, t2) (name3, t3)  = withType name <| map3 cons (field name1 t1) (field name2 t2) (field name3 t3)
branch4 : (a -> a1 -> a2 -> a3 -> value) -> String -> ( String, Decoder a ) -> ( String, Decoder a1 ) -> ( String, Decoder a2 ) -> ( String, Decoder a3 ) -> Decoder value
branch4 cons name (name1, t1) (name2, t2) (name3, t3) (name4, t4) = withType name <| map4 cons (field name1 t1) (field name2 t2) (field name3 t3) (field name4 t4)
branch5 : (a -> a1 -> a2 -> a3 -> a4 -> value) -> String -> ( String, Decoder a ) -> ( String, Decoder a1 ) -> ( String, Decoder a2 ) -> ( String, Decoder a3 ) -> ( String, Decoder a4 ) -> Decoder value
branch5 cons name (name1, t1) (name2, t2) (name3, t3) (name4, t4) (name5, t5) = withType name <| map5 cons (field name1 t1) (field name2 t2) (field name3 t3) (field name4 t4) (field name5 t5)
branch6 : (a -> a1 -> a2 -> a3 -> a4 -> a5 -> value) -> String -> ( String, Decoder a ) -> ( String, Decoder a1 ) -> ( String, Decoder a2 ) -> ( String, Decoder a3 ) -> ( String, Decoder a4 ) -> ( String, Decoder a5 ) -> Decoder value
branch6 cons name (name1, t1) (name2, t2) (name3, t3) (name4, t4) (name5, t5) (name6, t6) = withType name <| map6 cons (field name1 t1) (field name2 t2) (field name3 t3) (field name4 t4) (field name5 t5) (field name6 t6)

-- branch0DE cons name = (withType name <| succeed cons, objectE name []) -- Consistency...
-- branch1DE cons name (name1, d1, e1, a1)  = (withType name <| map cons (field name1 d1), \v -> objectE name [(name1, e1 (a1 v))])


--
-- Encoder helpers
--

dictE : (a -> E.Value) -> Dict String a -> E.Value
dictE enc dict = E.object <| toList <| Dict.map (always enc) dict

objectE : String -> List ( String, E.Value ) -> E.Value
objectE tag ps = E.object <| ("type", E.string tag) :: ps

listE : (a -> E.Value) -> List a -> E.Value
listE e lst = E.list <| List.map e lst

maybeE : (a -> E.Value) -> Maybe a -> E.Value
maybeE e m = case m of
               Nothing -> E.null
               Just i -> e i

-- A bunch of dictionaries/lists are expected to remain the same (or only grow) at least during action sequences.
-- Will mark those items with a "**" I guess

-- Positions and units are screen pixels

--
-- World state
-- Doubles as level description by excluding some in-motion stuff (MovingTo + InAnimation)
--

type alias Pos = { x: Float, y: Float }
pos : D.Decoder Pos
pos = branch2 Pos "pos" ("x", float) ("y", float)
posE : Pos -> E.Value
posE p = objectE "pos" [("x", E.float p.x), ("y", E.float p.y)]

type alias World = { character : Character
                   , currentScene : String 
                   , scenes : Dict String Scene -- **
                   , debug : Bool -- Not saved
                   , clickData : List Pos -- Not saved, For random debugging/design purposes...
                   }
world : Decoder World
world = branch3 (\a b c -> World a b c False []) "world" ("character", character) ("currentScene", string) ("scenes", dict scene)
worldE : World -> E.Value
worldE w = objectE "world" [ ("character", characterE w.character), ("currentScene", E.string w.currentScene)
                           , ("scenes", dictE sceneE w.scenes) ]

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
character : Decoder Character
character = withType "character" <| map8 Character (field "w" float) (field "h" float) (field "pos" pos) (succeed Still)
            (field "speed" float) (field "facing" facing) (field "walkCycle" animCycle) (field "inventory" <| list item)
characterE : Character -> E.Value
characterE c = objectE "character" [ ("w", E.float c.width), ("h", E.float c.height), ("pos", posE c.pos)
                                   , ("speed", E.float c.speed), ("facing", facingE c.facing), ("walkCycle", animCycleE c.walkCycle)
                                   , ("inventory", listE itemE c.inventory) ]

type alias Item = { width : Float
                  , height : Float
                  , img : String
                  , name : String -- Possibly debug-only...
                  }
item : Decoder Item
item = branch4 Item "item" ("w", float) ("h", float) ("img", string) ("name", string)
itemE : Item -> E.Value       
itemE i = objectE "item" [ ("w", E.float i.width), ("h", E.float i.height), ("img", E.string i.img)
                         , ("name", E.string i.name) ]

type Facing = Left | Right
facing : Decoder Facing
facing = oneOf [ branch0 Left "left", branch0 Right "right" ]
facingE : Facing -> E.Value
facingE f = case f of
                Left -> objectE "left" []
                Right -> objectE "right" []

type Action = None
            -- Item locations are the one basically hardcoded game element left
            -- Could expand them, could replace them
            | UseItemLocation String
            -- Make a Usable active (visible and clickable)
            | ActivateUsable String String
            | Leave String Int
            -- A very limited, 1-object-at-a-time form of animation
            | AnimateUsable String Float AnimCycle
            -- Stopgap. If action1 tries to animate etc, this just blows up
            | Sequence Action Action 
            -- Could be chopped up to separate the query from the actions
            | ContentsCheck (List (String, String, Maybe String)) Action Action
            | GoScene String
            | ReturnToMenu
action : D.Decoder Action
action = D.oneOf [ branch0 None "none"
                 , branch1 UseItemLocation "useItemLocation" ("itemLoc", string)
                 , branch2 ActivateUsable "activateUsable" ("scene", string) ("usable", string)
                 , branch2 Leave "leave" ("scene", string) ("spawn", int)
                 , branch3 AnimateUsable "animateUsable" ("usable", string) ("duration", float) ("animCycle", animCycle)
                 , branch2 Sequence "sequence" ("action1", lazy (\_ -> action)) ("action2", lazy (\_ -> action))
                 , branch3 ContentsCheck "contentsCheck" ("cond", list itemCheck) ("success", lazy (\_ -> action)) ("fail", lazy (\_ -> action)) -- TODO
                 , branch1 GoScene "goScene" ("scene", string)
                 , branch0 ReturnToMenu "returnToMenu"
                 ]
actionE : Action -> E.Value
actionE a = case a of
                None -> objectE "none" []
                UseItemLocation itemLoc -> objectE "useItemLocation" [("itemLoc", E.string itemLoc)]
                ActivateUsable scene usable -> objectE "activateUsable" [("scene", E.string scene), ("usable", E.string usable)]
                Leave scene spawn -> objectE "leave" [("scene", E.string scene), ("spawn", E.int spawn)]
                AnimateUsable usable duration animCycle ->
                    objectE "animateUsable" [("usable", E.string usable), ("duration", E.float duration), ("animCycle", animCycleE animCycle)]
                Sequence a1 a2 -> objectE "sequence" [("action1", actionE a1), ("action2", actionE a2)]
                ContentsCheck itemChecks s f ->
                    objectE "contentsCheck" [("cond", listE itemCheckE itemChecks), ("success", actionE s), ("fail", actionE f)]
                GoScene scene -> objectE "goScene" [("scene", E.string scene)]
                ReturnToMenu -> objectE "returnToMenu" []

itemCheck : Decoder (String, String, Maybe String)
itemCheck = branch3 (,,) "itemCheck" ("scene", string) ("itemLoc", string) ("item", maybe string)
itemCheckE : (String, String, Maybe String) -> E.Value
itemCheckE (scene, itemLoc, item) = objectE "itemCheck" [("scene", E.string scene), ("itemLoc", E.string itemLoc),
                                                             ("item", maybeE E.string item)]

type alias AnimCycle = List (String, Time)
animCycle : Decoder AnimCycle
animCycle = list (branch2 (,) "animCycle" ("img", string) ("duration", float))
animCycleE : AnimCycle -> E.Value
animCycleE ac = listE (\(img, dur) -> objectE "animCycle" [("img", E.string img), ("duration", E.float dur)]) ac

type alias Cursor = String    

type alias Scene = { image : String
                   , playfields : List Playfield -- **
                   , entrance : List Pos -- **
                   , itemLocations : Dict String ItemLocation -- **
                   , usables : Dict String Usable -- ** Someday this and itemLocations (and maybe exits and walkables) will move together...
                   }
scene : Decoder Scene
scene = branch5 Scene "scene" ("img", string) ("fields", list playfield) ("entrances", list pos) ("itemLocs", dict itemLocation) ("usables", dict usable)
sceneE : Scene -> E.Value
sceneE s = objectE "scene" [ ("img", E.string s.image), ("fields", listE playfieldE s.playfields), ("entrances", listE posE s.entrance)
                           , ("itemLocs", dictE itemLocE s.itemLocations), ("usables", dictE usableE s.usables) ]

-- General purpose on-screen item location...
-- Might be re-usable for inventory...
-- For now, any item can be put "anywhere"...
-- Later, some items will be pick-up only, fit specific item types, etc.
type alias ItemLocation = { field : Playfield
                          , contents : Maybe Item
                          , collectPoint : Pos -- Point to walk to before interacting, should be in a field
                          }
itemLocation : Decoder ItemLocation
itemLocation = branch3 ItemLocation "itemLocation" ("field", playfield) ("contents", maybe item) ("collectPoint", pos)
itemLocE : ItemLocation -> E.Value               
itemLocE il = objectE "itemLocation" [("field", playfieldE il.field), ("contents", maybeE itemE il.contents), ("collectPoint", posE il.collectPoint)]
                                                                           
type alias Usable = { field : Playfield
                    , event : Action
                    , usePoint : Maybe Pos
                    , img : Maybe String -- Probably changeable...
                    , cursor : Cursor
                    , active : Bool
                    }
usable : Decoder Usable
usable = branch6 Usable "usable" ("field", playfield) ("event", action) ("usePoint", maybe pos) ("img", maybe string) ("cursor", string) ("active", bool)
usableE : Usable -> E.Value         
usableE u = objectE "usable" [ ("field", playfieldE u.field), ("event", actionE u.event), ("usePoint", maybeE posE u.usePoint)
                             , ("img", maybeE E.string u.img), ("cursor", E.string u.cursor), ("active", E.bool u.active)]

-- Playfield segment rectangle
-- Needs to be called something else...
type alias Playfield = { width : Float
                       , height : Float
                       , x : Float
                       , y : Float
                       }
playfield : Decoder Playfield
playfield = branch4 Playfield "playfield" ("w", float) ("h", float) ("x", float) ("y", float)
playfieldE : Playfield -> E.Value            
playfieldE pf = objectE "playfield" [("w", E.float pf.width), ("h", E.float pf.height), ("x", E.float pf.x), ("y", E.float pf.y)]

--
-- "Runtime" types
--

-- MovingTo - multi-segment movement plan
type CharState = Still
               | MovingTo (List Pos) InAnimation Action

-- Very simple animation system
-- Or it was supposed to be...
type alias InAnimation = { segments : AnimCycle
                         , current : AnimCycle
                         }

type GameAnimation = AnimationUsable String Float InAnimation (Maybe String) Action

