module GameState exposing(..)

import Dict exposing (Dict)
import Time exposing (Time)

import Json.Decode as D exposing (..)

-- Quick and dirty object decoders with explicit type
branch0 : a -> String -> Decoder a
branch0 cons name = map (always cons) (field name string)
branch1 : (a -> value) -> String -> ( String, Decoder a ) -> Decoder value
branch1 cons name (name1, t1)  = map2 (always cons) (field name string) (field name1 t1)
branch2 : (a -> a1 -> value) -> String -> ( String, Decoder a ) -> ( String, Decoder a1 ) -> Decoder value
branch2 cons name (name1, t1) (name2, t2)  = map3 (always cons) (field name string) (field name1 t1) (field name2 t2)
branch3 : (a -> a1 -> a2 -> value) -> String -> ( String, Decoder a ) -> ( String, Decoder a1 ) -> ( String, Decoder a2 ) -> Decoder value
branch3 cons name (name1, t1) (name2, t2) (name3, t3)  = map4 (always cons) (field name string) (field name1 t1) (field name2 t2) (field name3 t3)
branch4 : (a -> a1 -> a2 -> a3 -> value) -> String -> ( String, Decoder a ) -> ( String, Decoder a1 ) -> ( String, Decoder a2 ) -> ( String, Decoder a3 ) -> Decoder value
branch4 cons name (name1, t1) (name2, t2) (name3, t3) (name4, t4) = map5 (always cons) (field name string) (field name1 t1) (field name2 t2) (field name3 t3) (field name4 t4)
branch5 : (a -> a1 -> a2 -> a3 -> a4 -> value) -> String -> ( String, Decoder a ) -> ( String, Decoder a1 ) -> ( String, Decoder a2 ) -> ( String, Decoder a3 ) -> ( String, Decoder a4 ) -> Decoder value
branch5 cons name (name1, t1) (name2, t2) (name3, t3) (name4, t4) (name5, t5) = map6 (always cons) (field name string) (field name1 t1) (field name2 t2) (field name3 t3) (field name4 t4) (field name5 t5)
branch6 : (a -> a1 -> a2 -> a3 -> a4 -> a5 -> value) -> String -> ( String, Decoder a ) -> ( String, Decoder a1 ) -> ( String, Decoder a2 ) -> ( String, Decoder a3 ) -> ( String, Decoder a4 ) -> ( String, Decoder a5 ) -> Decoder value
branch6 cons name (name1, t1) (name2, t2) (name3, t3) (name4, t4) (name5, t5) (name6, t6) = map7 (always cons) (field name string) (field name1 t1) (field name2 t2) (field name3 t3) (field name4 t4) (field name5 t5) (field name6 t6)

-- A bunch of dictionaries/lists are expected to remain the same (or only grow) at least during action sequences.
-- Will mark those items with a "**" I guess

-- Positions and units are screen pixels

--
-- World state
-- Doubles as level description
-- 

type alias Pos = { x: Float, y: Float }
pos : D.Decoder Pos
pos = D.map2 Pos (D.field "x" D.float) (D.field "y" D.float)      

type alias World = { character : Character
                   , currentScene : String 
                   , scenes : Dict String Scene -- **
                   , debug : Bool
                   , clickData : List Pos -- For random debugging/design purposes...
                   }
world : Decoder World
world = branch5 World "world" ("character", character) ("currentScene", string) ("scenes", dict scene) ("debug", bool) ("clickData", list pos)

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
character = map8 Character (field "w" float) (field "h" float) (field "pos" pos) (field "state" charState)
            (field "speed" float) (field "facing" facing) (field "walkCycle" animCycle) (field "inventory" <| list item)

type alias Item = { width : Float
                  , height : Float
                  , img : String
                  , name : String -- Possibly debug-only...
                  }
item : Decoder Item
item = branch4 Item "item" ("w", float) ("h", float) ("img", string) ("name", string)

type Facing = Left | Right
facing : Decoder Facing
facing = oneOf [ branch0 Left "left", branch0 Right "right" ]

-- MovingTo - multi-segment movement plan
type CharState = Still
               -- Skip out of encoding MovingTo and InAnimation for now.
               | MovingTo (List Pos) InAnimation Action
charState : Decoder CharState
charState = branch0 Still "still"

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

itemCheck : Decoder (String, String, Maybe String)
itemCheck = branch3 (,,) "itemCheck" ("scene", string) ("itemLoc", string) ("item", maybe string)

type alias AnimCycle = List (String, Time)
animCycle : Decoder AnimCycle
animCycle = list (branch2 (,) "animCycle" ("img", string) ("duration", float))

type alias Cursor = String    

type alias Scene = { image : String
                   , playfields : List Playfield -- **
                   , entrance : List Pos -- **
                   , itemLocations : Dict String ItemLocation -- **
                   , usables : Dict String Usable -- ** Someday this and itemLocations (and maybe exits and walkables) will move together...
                   }
scene : Decoder Scene
scene = branch5 Scene "scene" ("img", string) ("fields", list playfield) ("entrances", list pos) ("itemLocs", dict itemLocation) ("usables", dict usable)

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

type alias Usable = { field : Playfield
                    , event : Action
                    , usePoint : Maybe Pos
                    , img : Maybe String -- Probably changeable...
                    , cursor : Cursor
                    , active : Bool
                    }
usable : Decoder Usable
usable = branch6 Usable "usable" ("field", playfield) ("event", action) ("usePoint", maybe pos) ("img", maybe string) ("cursor", string) ("active", bool)

-- Playfield segment rectangle
-- Needs to be called something else...
type alias Playfield = { width : Float
                       , height : Float
                       , x : Float
                       , y : Float
                       }
playfield : Decoder Playfield
playfield = branch4 Playfield "playfield" ("w", float) ("h", float) ("x", float) ("y", float)            

--
-- "Runtime" types
--

type GameState = Menu
               | Interact World -- We're playing
               | Animate GameAnimation World

-- Very simple animation system
-- Or it was supposed to be...
type alias InAnimation = { segments : AnimCycle
                         , current : AnimCycle
                         }

type GameAnimation = AnimationUsable String Float InAnimation (Maybe String) Action

