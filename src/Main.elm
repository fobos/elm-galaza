import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Color exposing (..)
import Time exposing (..)
import Window
import Keyboard


-- MODEL
(gameWidth, gameHeight) = (600,400)
(halfWidth, halfHeight) = (300,200)

type alias Game =
  { ship : Ship
  }

type alias Ship =
  { x : Float  -- just 1 degree of freedom (left-right)
  , vx : Float  -- either 0, 1 or -1
  , shooting : Bool
  }

initShip : Ship
initShip =
  { x = 0
  , vx = 0
  , shooting = False
  }

defaultGame : Game
defaultGame =
  { ship = initShip
  }

type alias Keys = { x : Int, y : Int }

-- UPDATE
applyPhysics : Float -> Ship -> Ship
applyPhysics dt ship =
    { ship | x = ship.x + ship.vx * dt }

updateShip : (Float, Keys) -> Ship -> Ship
updateShip (dt, dir) ship =
  let
    isShooting  = dir.y > 0
    movedShip = applyPhysics dt { ship | vx = toFloat dir.x }
  in
    { movedShip |
        x = clamp (27 - halfWidth) (halfWidth - 27) movedShip.x,
        shooting = isShooting
    }

update : (Float, Keys) -> Game -> Game
update input game =
  {game | ship = (updateShip input game.ship) }

-- SIGNALS
inputSignal : Signal (Float, Keys)
inputSignal =
  let delta = fps 35
      tuples = Signal.map2 (,) delta Keyboard.arrows
  in  Signal.sampleOn delta tuples

-- VIEW

view : (Int, Int) -> Game -> Element
view (w, h) game =
  flow down [ show game.ship,
  container w h middle <|
    collage gameWidth gameHeight
      [ rect gameWidth gameHeight
          |> filled (rgb 60 100 60)
      --, rect 50 10
      --    |> make game.ship
      , polygon [(0,0), (0,10), (10,10), (10,20), (20,20), (20,25), (25,25), (25,20), (35,20), (35,10), (45,10), (45,0)]
          |> make game.ship
      ]
      ]


--make : a -> Shape -> Form
make obj shape =
  shape
    |> filled white
    |> move (obj.x, (10 - halfHeight))




main : Signal Element
--main = Signal.map show (Signal.foldp updateShip initShip inputSignal)
main = Signal.map2 view Window.dimensions (Signal.foldp update defaultGame inputSignal)
