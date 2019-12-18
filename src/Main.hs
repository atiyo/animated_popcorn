module Main where
import Data.Array.Base
import Data.Array.Unboxed
import Data.Array.ST
import Data.Maybe
import Control.Monad
import Control.Monad.ST
import Codec.Picture

sideLength = 128 :: Int
h = 0.046 :: Double
reps = 8
maxInd = sideLength ^ 2 - 1
numPoints = sideLength ^ 2


type H = Double
type Ind = (Int, Int)
type Histogram = UArray Ind Double
type Point = (Double, Double)
type Points = (UArray Int Double, UArray Int Double)
type System = (Points, Histogram)


initPoints :: Points
initPoints = (makeCoords xs, makeCoords ys)
  where stepSize = 2 * pi / fromIntegral sideLength
        singleDim = take sideLength [-pi, -pi + stepSize..]
        values = (,) <$> singleDim <*> singleDim
        xs = [fst value | value <- values]
        ys = [snd value | value <- values]
        makeCoords = listArray (0, maxInd)

initHistogram :: Histogram
initHistogram = runSTUArray $ newArray ((0, 0), (maxInd, maxInd)) 0
  

updatePoint :: Monad m =>  H -> Point -> m Point
updatePoint h (x, y) = return (newX, newY)
  where newX = x - h * sin (y + tan (3 * y))
        newY = y - h * sin (x + tan (3 * x))

updatePoints :: H -> Points -> Points
updatePoints h points =  runST $ do
  let (xs, ys) =  points
  thawX <- thaw xs
  thawY <- thaw ys
  forM_ (take numPoints [0..]) $ \i -> do
    oldX <- readArray thawX i
    oldY <- readArray thawY i
    (newX, newY) <- updatePoint h (oldX, oldY)
    writeArray thawX i newX
    writeArray thawY i newY
  liftM2 (,) (unsafeFreezeSTUArray thawX) (unsafeFreezeSTUArray thawY)

pointToInd :: Monad m => Point -> m Ind
pointToInd (x, y) = return (toInd x,  toInd y)
  where norm z = mod z maxInd
        toInt z = floor $ fromIntegral sideLength * (z + pi) / (2 * pi)
        toInd = norm . toInt
    
updateHistogram :: Histogram -> Points -> Histogram
updateHistogram hist points = runSTUArray $ do
  let (xs, ys) =  points
  --thawX <- thaw xs
  --thawY <- thaw ys
  thawHist <- thaw hist
  forM_ (take numPoints [0..]) $ \i -> do
    let xVal = xs ! i
    let yVal = ys ! i
    newInd <- pointToInd (xVal, yVal)
    currentVal <- readArray thawHist newInd
    writeArray thawHist newInd (currentVal + 1)
  --unsafeFreezeSTUArray thawX
  --unsafeFreezeSTUArray thawY
  return thawHist
       
updateSystem :: H -> System -> System
updateSystem h (points, hist) = (newPoints, newHist)
  where newPoints = updatePoints h points
        newHist = updateHistogram hist newPoints

simulate :: H -> Int -> System
simulate h n
  | n > 0 = updateSystem h $ simulate h (n - 1)
  | otherwise = (initPoints, initHistogram)

histToImage :: Histogram -> Image Pixel8
histToImage hist = generateImage f sideLength sideLength
  where maxValue = foldl max 0 $ elems hist
        val x y = hist ! (x, y)
        f x y = floor $ 255 * val x y / maxValue

writeOutput :: Histogram -> FilePath -> IO ()
writeOutput hist filePath = saveBmpImage filePath $ ImageY8 image
  where image = histToImage hist

main :: IO ()
--main = putStrLn "yo!"
main = do
  let (_, hist) = simulate h reps
  writeOutput hist "./test.bmp"
