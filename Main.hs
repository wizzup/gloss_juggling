-- | Original code at https://cdsmith.wordpress.com/2012/02/14/juggling-in-haskell-and-gloss/

module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort

-- * Types

type Hand    = Float
type Time    = Float
type Pattern = [Int]
type Ball    = (Float,Float,Int)
type Balls   = [Ball]
type Model   = (Hand, Time, Balls, Pattern)

-- * Pictures

-- ** main drawings

-- | draw model (juggler and balls)
draw :: Model -> Picture
draw (_, _, balls, _) = pictures
    [ juggler
    , pictures $ zipWith color colors [ ball b | b <- balls ]
    -- , pictures $ zipWith3 ball' [1..] colors balls
    -- , ballCount balls
    ]

-- | ball with color and number
ball' :: Int -> Color -> Ball -> Picture
ball' i c (bhand, btime, height)
  = translate (50*x) (50*y)
  $ pictures
  [ color c $ circle 10
  , translate (-4) (-5) . scale 0.1 0.1 . text $ show i
  ]
  where
    t = 1 - 2 * (btime / fromIntegral height)
    x = if even height then bhand else bhand * t
    y = if height < 3 then 0 else fromIntegral (height - 1) * (1 - t**2)

-- | picture of a given ball
ball :: Ball -> Picture
ball (bhand, btime, height) = translate (50*x) (50*y) (circleSolid 10)
    where t = 1 - 2 * (btime / fromIntegral height)
          x = if even height then bhand else bhand * t
          y = if height < 3 then 0 else fromIntegral (height - 1) * (1 - t**2)

-- | stick figure of a juggler
juggler :: Picture
juggler = pictures [
    line [(-50, 0), (0, 25), (50, 0)],
    line [(-30, -100), (0, -50), (30, -100)],
    line [( 0, 25), (0, -50)],
    translate 0 50 (circle 25)
    ]

-- ** debuging

-- | number of balls (for debugging)
ballCount :: Balls -> Picture
ballCount bs = translate 100 100
             . scale 0.5 0.5
             . text $ "ball count: " ++ (show $ length bs)

-- ** misc

-- | infinite list of built-in color palate from
-- <https://hackage.haskell.org/package/gloss/docs/Graphics-Gloss-Data-Color.html#g:3 gloss>
colors :: [Color]
colors = cycle [red, green, blue, yellow, cyan, magenta, rose
               , violet, azure, aquamarine, chartreuse, orange]

-- * Simulation

-- | juggling pattern
thePattern :: [Int]
thePattern = [5,2,5,1,2]

-- | initial state
initial :: Model
initial = (-1, 0.0, [], cycle thePattern)

-- | simulation step
step :: Time -> Model -> Model
step dt (hand, time, balls, pattern) = (newhand, newtime, newballs, newpattern)
    where (throw, newtime) = properFraction (time + dt)
          newhand    = if throw == 1 then -hand else hand
          thrown     = if throw == 1 then [ (hand, newtime, head pattern) ]
                                     else []
          newpattern = if throw == 1 then tail pattern else pattern
          newballs   = [ (bhand, btime + dt, height)
                       | (bhand, btime, height) <- balls,
                         btime + dt < fromIntegral height ]
                       ++ thrown

-- ** misc

iter :: ViewPort -> Time -> Model -> Model
iter _ f m = step f m

main :: IO ()
main = simulate FullScreen white 30 initial draw iter
