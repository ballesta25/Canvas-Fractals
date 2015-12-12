-- Andrei Elliott
-- EECS 776
-- Semester project
--
-- Fractal drawing program in Blank Canvas

{-# language GADTs #-}
import Graphics.Blank
import Data.Function

type Point = (Double, Double)

type Line = (Point, Point)

-- Apply function coordinate-wise to a pair.  Useful for vector addition
liftPair :: (a -> b -> c) -> (a, a) -> (b, b) -> (c, c)
liftPair f (w, x) (y, z) = (f w y, f x z)

-- Euclidean distance
dist :: Point -> Point -> Double
dist a b = sqrt . uncurry ((+) `on` (^2)) $ liftPair (-) a b

type Angle = Double

-- Angle of the slope from point a to point b.  Mathemeticians coordinates: radians from the x-axis starting toward the direction of y.  In this case that means clockwise from right
angle :: Point -> Point -> Angle
angle a b = (uncurry (flip atan2)) (liftPair (-) b a)

-- returns a Point at distance r from the origin at given angle
polar :: Double -> Angle -> Point
polar r angle = (r * cos angle, r * sin angle)

drawLine :: Line -> Canvas ()
drawLine (start, end) = do
    beginPath()
    moveTo start
    lineTo end
    stroke()

data LineFractal where
    LineFractal :: Line -> [LineFractal] -> LineFractal

mkFractal :: (Line -> [Line]) -> Line -> LineFractal
mkFractal f start = LineFractal start (map (mkFractal f) (f start))


class Fractal f where
    approximate :: f -> Canvas () -- draw a simple approximation to the fractal (e.g. a line segment)
    step :: f -> [f]

instance Fractal LineFractal where
    approximate (LineFractal l _)    = drawLine l
    step        (LineFractal _ next) = next

-- draw the approximations for the fractal a given depth (only at that depth, 
--   no previous levels are drawn)
drawLeaves :: Fractal f => Integer -> f -> Canvas ()
drawLeaves 0     = approximate
drawLeaves depth = mapM_ (drawLeaves (depth - 1)) . step

-- recurrence for the Koch Snowflake
{-

                            e
    a---------b  --->      / \
                       a--c   d--b

-}
koch :: Line -> [Line]
koch (a, b) = let r   = dist  a b
                  phi = angle a b
                  c   = liftPair (+) a (polar (r/3) phi)
                  d   = liftPair (+) a (polar (2*r/3) phi)
                  e   = liftPair (+) c (polar (r/3) (phi + (pi/3)))
                  chain = [a,c,e,d,b]
              in zip chain (tail chain)

main :: IO()
main = blankCanvas 3000 $ \context -> do 
         send context $ drawLeaves 5 $ mkFractal koch ((100,200),(1000,200))
           
