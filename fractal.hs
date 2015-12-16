-- Andrei Elliott
-- EECS 776
-- Semester project
--
-- Fractal drawing program in Blank Canvas
{-
    Fractals are represented here as lazy infinite trees.  The primary means of interacting with a fractal are the functions `step`, which returns the next level of the tree as a list and `approximate` which returns a Canvas() that draws an approximation to the 'rest of' the fractal at the current level - this is needed in order to bottom-out the recursion when drawing.
    The mkFractal function generates Fractal data structures given a function that converts from a single line to the next level of the fractal.  It applies this function recursively to every line in the fractal.  I have also implemented a union constructor, (:+), which combines two Fractals into a single Fractal object.  It would be fairly simple to add a constructor based on arbitrary Canvas objects to allow fractals with components other than lines.
    The drawLeaves and drawBranches functions both draw a representation of a given Fractal to a given depth, the first under the assumption that each level replaces the one before, and the second under the assumption that it is added in instead.
-}
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

{- Angle of the slope from point a to point b.  Mathemetician's coordinates: radians from the x-axis starting toward the direction of y.  In this case that means clockwise from right-}
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

data Fractal where
    LineFractal :: Line -> [Fractal] -> Fractal
    (:+) :: Fractal -> Fractal -> Fractal -- union

mkFractal :: (Line -> [Line]) -> Line -> Fractal
mkFractal f start = LineFractal start (map (mkFractal f) (f start))

-- draw a simple approximation to the fractal (e.g. a line segment)
approximate :: Fractal -> Canvas () 
approximate (LineFractal l _) = drawLine l
approximate (f :+ g) = approximate f >> approximate g

step :: Fractal -> [Fractal]
step (LineFractal _ next) = next
step (f :+ g) = step f ++ step g

-- draw the approximations for the fractal at the given depth (only at that depth,
--   no previous levels are drawn)
drawLeaves :: Int -> Fractal -> Canvas ()
drawLeaves 0     = approximate
drawLeaves depth = mapM_ (drawLeaves (depth - 1)) . step

-- draw the approximations at all levels up to given depth
drawBranches :: Int -> Fractal -> Canvas ()
drawBranches 0     f = approximate f
drawBranches depth f = approximate f >> mapM_ (drawBranches (depth - 1)) (step f)

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

snowflake :: Line -> Fractal
snowflake (a, b) = let c = liftPair (+) a $
                           polar (dist a b) $ (angle a b) + pi/3
                   in foldr1 (:+) $ map (mkFractal koch) [(a, b), (b, c), (c, a)]

htree :: Double -> Line -> [Line]
htree scale (a, b) = let r   = scale * dist a b
                         phi = angle a b
                     in [(b, liftPair (+) b (polar r (phi + pi/2)))
                        ,(b, liftPair (+) b (polar r (phi - pi/2)))]

-- has the Cantor set as its limit
cantor :: Line -> [Line]
cantor (a, b) = [(a, oneThirdFromTo a b), (b, oneThirdFromTo b a)]
    where oneThirdFromTo = liftPair (\x y -> (x+x + y)/3)

-- Draw several iterations of a fractal at given offset from each other
generations :: Int -> Point -> Fractal -> Canvas ()
generations limit offset  f = sequence_ 
            [drawLeaves n f >> translate offset | n <- [0..limit-1]] 
            >> translate (undo * fst offset, undo * snd offset)
            where undo = (-1) * fromIntegral limit -- remove the offset from later drawings

main :: IO()
main = blankCanvas 3000 $ \context -> do 
         send context $ do 
           drawLeaves 5 $ mkFractal koch ((100,400),(1000,400))
           generations 6 (0, 50) (mkFractal cantor ((200, 500), (800, 500)))
           drawLeaves 4 $ snowflake ((1400,200),(1500,200))
           drawBranches 8 $ mkFractal (htree $ 1/sqrt 2) ((1250,600),(1200, 500))
           drawBranches 3 $ mkFractal ((concatMap $ htree 0.5) . koch) ((50,200),(250,200))
           drawBranches 3 $ mkFractal ((concatMap $ koch) . htree 0.5) ((750,200),(950,200))