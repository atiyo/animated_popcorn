{-# LANGUAGE Strict #-}
module Main where
import qualified Data.Map as M
--import qualified Data.Map.Strict as M
import qualified Codec.Picture as P
import Data.Maybe

sideLength = 512 :: Int
h = 0.037 :: Double
reps = 8 
side = take sideLength [0..]
validInds = [(x, y) | x <- side, y <- side]


type H = Double
type Histogram = M.Map (Int, Int) Double
type Point = (Double, Double)
type Points = [Point]
type View = P.Image P.Pixel8
type System = (Points, Histogram)
type Ind = (Int, Int)
type Inds = [Ind]


initHistogram :: Histogram
initHistogram = M.fromList keyValues
  where inds = (,) <$> side <*> side
        keyValues = [(key, 0.0) | key <- inds]

initPoints :: Points
initPoints = values
  where stepSize = 2 * pi / fromIntegral sideLength
        singleDim = take sideLength [-pi, -pi + stepSize..]
        values = (,) <$> singleDim <*> singleDim

initSystem :: System
initSystem = (initPoints, initHistogram)

updatePoint :: H -> Point -> Point
updatePoint h (x, y) = (newX, newY)
  where newX = x - h * sin (y + tan (3 * y))
        newY = y - h * sin (x + tan (3 * x))

updatePoints :: H -> Points -> Points
updatePoints param  = map $ updatePoint param

pointToInd :: Point -> Ind
pointToInd (x, y) = (toInd x,  toInd y)
  where toInd z = floor $ fromIntegral sideLength * (z + pi) / (2 * pi)

pointsToInds :: Points -> Inds
pointsToInds = map pointToInd

countInds :: Inds -> Histogram
countInds inds = M.fromListWith (+) tupled
  where valid x = x `elem` validInds
        tupled = [(ind, 1.0) | ind <- inds, valid ind]

updateHistogram :: Histogram -> Histogram -> Histogram
updateHistogram = M.unionWith (+)

updateSystem :: H -> System -> System
updateSystem param (points, hist) = (newPoints, newHist)
  where newPoints = updatePoints param points
        incHist = countInds (pointsToInds newPoints)
        newHist = updateHistogram hist incHist

histToImage :: Histogram -> P.Image P.Pixel8
histToImage hist = P.generateImage f sideLength sideLength
  where maxValue = M.foldr max 0 hist
        val x y = fromMaybe 0 $ M.lookup (x, y) hist
        f x y = floor $ 255 * val x y / maxValue

writeOutput :: System -> FilePath -> IO ()
writeOutput (_, hist) filePath = P.saveBmpImage filePath $ P.ImageY8 image
  where image = histToImage hist

repFun :: (a -> a) -> Int -> a -> a
repFun f n a
  | n > 1 = f $ repFun f (n - 1) a
  | otherwise = f a

main :: IO ()
main = writeOutput final "./test.bmp"
  where final = repFun (updateSystem h) reps initSystem 
