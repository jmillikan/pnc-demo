module Expect exposing (..)

import Dict exposing (Dict, get)
import Result exposing (andThen)

-- Expect: Some unnatural stuff for moving around in dictionaries/lists by key/index

-- expectAt and expectIn are different ways round. It's bad.
-- Should just toss the lists and use Dict, but I have too much brain load right now                           
expectAt : (a -> Bool) -> List a -> Result String a
expectAt f l = Result.fromMaybe ("Expected to find item in " ++ toString l)
                <| List.head <| List.filter f l

-- Start clearing out the Maybe thicket...             
expectIn : Dict comparable v -> comparable -> Result String v
expectIn dict k = Result.fromMaybe ("Missing expected value for key " ++ toString k)
                  <| get k dict

                      -- Update by explicitly failing instead of silently failing
expectUpdate : comparable -> Dict comparable a -> (a -> Result String a) -> Result String (Dict comparable a)
expectUpdate key dict f =
    expectIn dict key
        |> andThen f
        |> Result.map (\v -> Dict.insert key v dict)


