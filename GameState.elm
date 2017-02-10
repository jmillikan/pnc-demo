module GameState exposing(..)

import Dict exposing (Dict)
import Time exposing (Time)

-- Positions and units are screen pixels
type alias Pos = { x: Float, y: Float }

type alias State = { character : Character
                   , currentScene : String
                   , scenes : Dict String Scene
                   , debug : Bool
                   , clickData : List Pos -- For random debugging/design purposes...
                   }

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
type CharState = Still
               | MovingTo (List Pos) InAnimation Action

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
