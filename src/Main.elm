import Graphics.Element exposing (..)
import Time exposing (..)
import Keyboard

-- MODEL
type alias Ship =
  { position : Float  -- just 1 degree of freedom (left-right)
  , velocity : Float  -- either 0, 1 or -1
  , shooting : Bool
  }

initShip : Ship
initShip =
  { position = 0
  , velocity = 0
  , shooting = False
  }

type alias Keys = { x : Int, y : Int }

-- UPDATE
applyPhysics : Float -> Ship -> Ship
applyPhysics dt ship =
  { ship | position = ship.position + ship.velocity * dt }

updateVelocity : Float -> Ship -> Ship
updateVelocity newVelocity ship =
  { ship | velocity = newVelocity }

updateShooting : Bool -> Ship -> Ship
updateShooting isShooting ship =
  { ship | shooting = isShooting }

update : (Float, Keys) -> Ship -> Ship
update (dt, keys) ship =
  let newVel      = toFloat keys.x
      isShooting  = keys.y > 0
  in  updateVelocity newVel (updateShooting isShooting (applyPhysics dt ship))


-- SIGNALS
inputSignal : Signal (Float, Keys)
inputSignal =
  let delta = fps 30
      tuples = Signal.map2 (,) delta Keyboard.arrows
  in  Signal.sampleOn delta tuples

main : Signal Element
main = Signal.map show (Signal.foldp update initShip inputSignal)
