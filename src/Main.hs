module Main where
import qualified Data.Array.Repa as R
import qualified Codec.Picture as P
import qualified Data.Map as M
import Data.Maybe

sideLength = 64 :: Int
h = 0.046 :: Double
reps = 8 
side = take sideLength [0..]
validInds = [(x, y) | x <- side, y <- side]


type H = Double
type Histogram = M.Map (Int, Int) Double
type Point = (Double, Double)
type Points = R.Array R.D R.DIM1 Point
type View = P.Image P.Pixel8
type Ind = (Int, Int)
type Inds = R.Array R.D R.DIM1 Ind
type System = (Points, Inds)

initPoints :: Points
initPoints = R.fromFunction (R.Z R.:. numPoints) f
  where stepSize = 2 * pi / fromIntegral sideLength
        singleDim = take sideLength [-pi, -pi + stepSize..]
        values = (,) <$> singleDim <*> singleDim
        numPoints = sideLength ^ 2
        f (R.Z R.:. x) = values !! x

updatePoint :: H -> Point -> Point
updatePoint h (x, y) = (newX, newY)
  where newX = x - h * sin (y + tan (3 * y))
        newY = y - h * sin (x + tan (3 * x))

updatePoints :: H -> Points -> Points
updatePoints param  = R.map $ updatePoint param

pointToInd :: Point -> Ind
pointToInd (x, y) = (toInd x,  toInd y)
  where toInd z = floor $ fromIntegral sideLength * (z + pi) / (2 * pi)

pointsToInds :: Points -> Inds
pointsToInds = R.map pointToInd

updateSystems :: H -> [System] -> [System]
updateSystems h (x:xs) = (newPoints, newInds):x:xs
  where newPoints = updatePoints h $ fst x
        newInds = pointsToInds newPoints
updateSystems h [] = [(initPoints, pointsToInds initPoints)]

simulate :: H -> Int -> [System]
simulate h n = updateSystems h $ simulate h (n - 1)

generateHist :: [System] -> Histogram
generateHist systems = M.fromListWith (+) values
  where listDelayed = map snd systems
        listComputed = concatMap R.toList listDelayed
        values = [(value, 1.0) | value <- listComputed]

histToImage :: Histogram -> P.Image P.Pixel8
histToImage hist = P.generateImage f sideLength sideLength
  where maxValue = M.foldr max 0 hist
        val x y = fromMaybe 0 $ M.lookup (x, y) hist
        f x y = floor $ 255 * val x y / maxValue

writeOutput :: [System] -> FilePath -> IO ()
writeOutput systems filePath = P.saveBmpImage filePath $ P.ImageY8 image
  where image = histToImage . generateHist $ systems

main :: IO ()
--main = putStrLn "yo!"
main = writeOutput final "./test.bmp"
  where final = simulate h reps
