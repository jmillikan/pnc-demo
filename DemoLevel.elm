module DemoLevel exposing (demoState)

import GameState exposing (..)
import Dict exposing (Dict, fromList)
import Time exposing (millisecond)

demoState : World
demoState = World initChar "middle" sceneDict False []

initChar : Character
initChar = Character 120 180 (Pos 200 300) Still (0.2 / millisecond) Right [("1", 500 * millisecond), ("2", 500 * millisecond)] []

-- demoScene isn't as exciting as it sounds.
demoScene : Scene
demoScene = Scene
            "bg1"
            [Playfield 800 330 100 160, Playfield 510 100 890 350, Playfield 700 100 -590 280]
            [Pos 1100 400, Pos 50 350] 
            (fromList [])
            (fromList [ ("demo-lever", Usable
                            (Playfield 170 170 300 300)
                            (AnimateUsable "demo-lever" (4000 * millisecond) [("lever-left1", 500 * millisecond), ("lever-left2", 500 * millisecond)])
                            (Pos 250 350)
                            (Just "lever-left")
                            "pointer")
                      , ("east-exit", Usable
                             (Playfield 100 300 1250 150)
                             (LeaveUsable "east" 0)
                             (Pos 1275 400)
                             Nothing
                             "e-resize")
                      , ("west-exit", Usable
                             (Playfield 100 300 0 80)
                             (LeaveUsable "west" 0)
                             (Pos 50 330)
                             Nothing
                             "w-resize")
                        ])

scene2 : Scene
scene2 = Scene
         "bg2"
         [Playfield 200 100 0 150, Playfield 800 500 130 100]
         [Pos 50 200]
         (fromList [])
         (fromList [("west-exit", Usable (Playfield 70 200 0 50) (LeaveUsable "middle" 0) (Pos 50 200) Nothing "w-resize")
                   ])

scene3 : Scene
scene3 = Scene
         "bg3"
         [ Playfield 1000 200 200 350
         , { width = 100, height = 140, x = 590, y = 150 }
         , { width = 100, height = 140, x = 873, y = 150 }
         ]
         [ Pos 1150 450 ]
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
         (fromList [ ("east-exit", Usable (Playfield 100 250 1200 200) (LeaveUsable "middle" 1) (Pos 1150 450) Nothing "e-resize")
                   ])

scenes : List Scene
scenes = [demoScene, scene2, scene3]

sceneDict : Dict String Scene
sceneDict = Dict.fromList [("middle", demoScene), ("west", scene3), ("east", scene2)]

