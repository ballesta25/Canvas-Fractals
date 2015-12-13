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

data UnionFractal where
    (:+)  :: (Fractal f, Fractal g) => f -> g -> UnionFractal
    Empty :: UnionFractal

instance Fractal UnionFractal where
    approximate (f :+ g) = approximate f >> approximate g
    approximate Empty    = return ()
    step        (f :+ g) = map (:+ Empty) (step f) ++
                           map (Empty :+) (step g) -- inefficient
    step        Empty    = []

-- draw the approximations for the fractal a given depth (only at that depth,
--   no previous levels are drawn)
drawLeaves :: Fractal f => Int -> f -> Canvas ()
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
                  e   = liftPair (+) c (polar (r/3) (phi - pi/3))
                  chain = [a,c,e,d,b]
              in zip chain (tail chain)

snowflake :: Line -> UnionFractal
snowflake (a, b) = let c = liftPair (+) a $
                           polar (dist a b) $ (angle a b) + pi/3
                   in foldr (:+) Empty $ map (mkFractal koch) [(a, b), (b, c), (c, a)]

-- has the Cantor set as its limit
cantor :: Line -> [Line]
cantor (a, b) = [(a, oneThirdFromTo a b), (b, oneThirdFromTo b a)]
    where oneThirdFromTo = liftPair (\x y -> (x+x + y)/3)

-- Draw several iterations of a fractal at given offset from each other
generations :: Fractal f => f -> Int -> Point -> Canvas ()
generations f limit offset = sequence_ 
            [drawLeaves n f >> translate offset | n <- [0..limit-1]] 
            >> translate (undo * fst offset, undo * snd offset)
            where undo = (-1) * fromIntegral limit -- remove the offset from later drawings

main :: IO()
main = blankCanvas 3000 $ \context -> do 
         send context $ do 
           drawLeaves 5 $ mkFractal koch ((100,400),(1000,400))
           generations (mkFractal cantor ((200, 500), (1200, 500))) 5 (0, 50)
           drawLeaves 4 $ snowflake ((1400,200),(1500,200))