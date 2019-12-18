module Main where
import Data.Array.Base
import Data.Array.Unboxed
import Data.Array.ST
import Data.Maybe
import Control.Monad
import Control.Monad.ST
import Codec.Picture

sideLength = 1024 :: Int
h = 0.023 :: Double
reps = 128
maxInd = sideLength ^ 2 - 1
numPoints = sideLength ^ 2


type H = Double
type Ind = Int
type Coord = (Int, Int)
type Histogram = UArray Int Double
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
initHistogram = runSTUArray $ newArray (0, numPoints - 1) 0
  

updatePoint :: H -> Point -> Point
updatePoint h (x, y) = (newX, newY)
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
    (newX, newY) <- return $ updatePoint h (oldX, oldY)
    writeArray thawX i newX
    writeArray thawY i newY
  liftM2 (,) (unsafeFreezeSTUArray thawX) (unsafeFreezeSTUArray thawY)

pointToCoord :: Point -> Coord
pointToCoord (x, y) = (toInd x,  toInd y)
  where norm z = mod z sideLength
        toInt z = floor $ fromIntegral sideLength * (z + pi) / (2 * pi)
        toInd = norm . toInt

coordToInd :: Coord -> Ind
coordToInd (x, y) = x * sideLength + y

indToCoord :: Ind -> Coord
indToCoord i = (major, i - major * sideLength)
  where major = floor $ fromIntegral i / fromIntegral sideLength
    
updateHistogram :: Histogram -> Points -> Histogram
updateHistogram hist points = runST $ do
  let (xs, ys) =  points
  thawX <- thaw xs
  thawY <- thaw ys
  thawHist <- thaw hist
  forM_ (take numPoints [0..]) $ \i -> do
    xVal <- readArray thawX i
    yVal <- readArray thawY i
    newInd <- return (coordToInd . pointToCoord $ (xVal, yVal))
    currentVal <- readArray thawHist newInd
    writeArray thawHist newInd (currentVal + 1)
  _ <- unsafeFreezeSTUArray thawX
  _ <- unsafeFreezeSTUArray thawY
  unsafeFreezeSTUArray thawHist
       
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
  where norm = sum (elems hist) / fromIntegral numPoints
        val x y = hist ! coordToInd (x, y)
        f x y = floor $ 255 * val x y / norm

writeOutput :: Histogram -> FilePath -> IO ()
writeOutput hist filePath = saveJpgImage 95 filePath $ ImageY8 image
  where image = histToImage hist

main :: IO ()
--main = putStrLn "yo!"
main = do
  let (_, hist) = simulate h reps
  writeOutput hist "./test.jpg"
