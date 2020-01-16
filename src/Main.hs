module Main where
import Data.Array.Base
import Data.Array.ST
import Control.Monad
import Control.Monad.ST
import Codec.Picture
import Data.Word

type H = Double
type Ind = Int
type Coord = (Int, Int)
type Histogram = UArray Int Double
type Point = (Double, Double)
type Points = (UArray Int Double, UArray Int Double)
type System = (Points, Histogram)

data Config = Config {
  sideLength_ :: Int,
  h_ :: Double,
  reps_ :: Int,
  maxInd_ :: Int,
  numPoints_ :: Int,
  startColour_ :: (Word8, Word8, Word8),
  endColour_ :: (Word8, Word8, Word8)
  }

type GifConfig = [Config]

initPoints :: Config -> Points
initPoints conf = (makeCoords xs, makeCoords ys)
  where stepSize = 2 * pi / fromIntegral sideLength
        singleDim = take sideLength [-pi, -pi + stepSize..]
        values = (,) <$> singleDim <*> singleDim
        xs = [fst value | value <- values]
        ys = [snd value | value <- values]
        makeCoords = listArray (0, maxInd)
        sideLength = sideLength_ conf
        maxInd = maxInd_ conf

initHistogram :: Config -> Histogram
initHistogram conf = runSTUArray $ newArray (0, numPoints - 1) 0
  where numPoints = numPoints_ conf
  
updatePoint :: Config -> Point -> Point
updatePoint conf (x, y) = (newX, newY)
  where newX = x - h * sin (y + tan (3 * y))
        newY = y - h * sin (x + tan (3 * x))
        h = h_ conf

updatePoints :: Config -> Points -> Points
updatePoints conf points =  runST $ do
  let (xs, ys) =  points
      numPoints = numPoints_ conf
  thawX <- thaw xs
  thawY <- thaw ys
  forM_ (take numPoints [0..]) $ \i -> do
    oldX <- readArray thawX i
    oldY <- readArray thawY i
    (newX, newY) <- return $ updatePoint conf (oldX, oldY)
    writeArray thawX i newX
    writeArray thawY i newY
  liftM2 (,) (unsafeFreezeSTUArray thawX) (unsafeFreezeSTUArray thawY)

pointToCoord :: Config -> Point -> Coord
pointToCoord conf (x, y) = (toInd x,  toInd y)
  where norm z = mod z sideLength
        toInt z = floor $ fromIntegral sideLength * (z + pi) / (2 * pi)
        toInd = norm . toInt
        sideLength = sideLength_ conf

coordToInd :: Config -> Coord -> Ind
coordToInd conf (x, y) = x * sideLength + y
  where sideLength = sideLength_ conf

indToCoord :: Config -> Ind -> Coord
indToCoord conf i = (major, i - major * sideLength)
  where major = floor $ fromIntegral i / fromIntegral sideLength
        sideLength = sideLength_ conf
    
updateHistogram :: Config -> Histogram -> Points -> Histogram
updateHistogram conf hist points = runST $ do
  let (xs, ys) =  points
      numPoints = numPoints_ conf
  thawX <- thaw xs
  thawY <- thaw ys
  thawHist <- thaw hist
  forM_ (take numPoints [0..]) $ \i -> do
    xVal <- readArray thawX i
    yVal <- readArray thawY i
    newInd <- return (coordToInd conf . pointToCoord conf $ (xVal, yVal))
    currentVal <- readArray thawHist newInd
    writeArray thawHist newInd (currentVal + 1)
  _ <- unsafeFreezeSTUArray thawX
  _ <- unsafeFreezeSTUArray thawY
  unsafeFreezeSTUArray thawHist

updateSystem :: Config -> System -> System
updateSystem conf (points, hist) = (newPoints, newHist)
  where newPoints = updatePoints conf points
        newHist = updateHistogram conf hist newPoints

simulate :: Config -> Int -> System
simulate conf n
  | n > 0 = updateSystem conf $ simulate conf (n - 1)
  | otherwise = (initPoints conf, initHistogram conf)

sysToImage :: Config -> System -> Image PixelRGB16
sysToImage conf (_, hist) = generateImage f sideLength sideLength
  where norm = sum (elems hist) / fromIntegral numPoints
        val x y = hist ! coordToInd conf (x, y)
        f x y = PixelRGB16 (256 * red x y) (256 * green x y) (256 * blue x y) 
        red x y = fromIntegral r0 + floor (fromIntegral (r1 - r0) * val x y / norm)
        green x y = fromIntegral g0 + floor (fromIntegral (g1 - g0) * val x y / norm)
        blue x y = fromIntegral b0 + floor (fromIntegral (b1 - b0) * val x y / norm)
        sideLength = sideLength_ conf
        numPoints = numPoints_ conf
        (r0, g0, b0) = startColour_ conf
        (r1, g1, b1) = endColour_ conf

--A lot of duplication below. Probably a good way to factor it into above with
--an Either, but copying and pasting was much easier.
sysToGifImage :: Config -> System -> Image PixelRGB8
sysToGifImage conf (_, hist) = generateImage f sideLength sideLength
  where norm = sum (elems hist) / fromIntegral numPoints
        val x y = hist ! coordToInd conf (x, y)
        f x y = PixelRGB8 (red x y) (green x y) (blue x y) 
        red x y =  r0 + floor (fromIntegral (r1 - r0) * val x y / norm)
        green x y =  g0 + floor (fromIntegral (g1 - g0) * val x y / norm)
        blue x y =  b0 + floor (fromIntegral (b1 - b0) * val x y / norm)
        sideLength = sideLength_ conf
        numPoints = numPoints_ conf
        (r0, g0, b0) = startColour_ conf
        (r1, g1, b1) = endColour_ conf

writeImage :: Config -> System -> FilePath -> IO ()
writeImage conf sys filePath = saveJpgImage 85 filePath $ ImageRGB16 image
  where image = sysToImage conf sys

genGifConfig :: IO GifConfig
genGifConfig = do
  putStrLn "Choose a lower value for h (around 0.025 is sensible)"
  lowerH <- read <$> getLine
  putStrLn "Choose a higher value for h (around 0.025 is sensible)"
  upperH <- read <$> getLine
  putStrLn "Choose the image size."
  sideLength <- read <$> getLine
  putStrLn "Choose the number of steps to simulate (128-256 normally works)."
  reps <- read <$> getLine
  putStrLn "Choose the start colour for the colour map as a tuple of Ints (0-255, 0-255, 0-255)"
  startColour <- read <$> getLine
  putStrLn "Choose a end color for the colour map as a tuple of Ints (0-255, 0-255, 0-255)"
  endColour <- read <$> getLine
  let oneConfig x = Config {sideLength_ = sideLength,
                            h_ = x,
                            reps_ = reps,
                            maxInd_ = sideLength ^ 2 - 1,
                            numPoints_ = sideLength ^ 2,
                            startColour_ = startColour,
                            endColour_ = endColour}
      amplitude = upperH - lowerH
      zeroTo2Pi = map (\x -> x * pi / 32) $ take 64 [0..]
      steps = map (\x -> lowerH + amplitude * (1 + cos x)) zeroTo2Pi
  return $ map oneConfig steps


genImageConfig :: IO Config
genImageConfig = do
  putStrLn "Choose a value for h (around 0.025 is sensible)"
  h <- read <$> getLine
  putStrLn "Choose the image size."
  sideLength <- read <$> getLine
  putStrLn "Choose the number of steps to simulate (128-256 normally works)."
  reps <- read <$> getLine
  putStrLn "Choose a starting color for the colour map in the form (0-255, 0-255, 0-255)"
  startColour <- read <$> getLine
  putStrLn "Choose an ending color for the colour map in the form (0-255, 0-255, 0-255)"
  endColour <- read <$> getLine
  return $ Config {sideLength_ = sideLength,
                   h_ = h,
                   reps_ = reps,
                   maxInd_ = sideLength ^ 2 - 1,
                   numPoints_ = sideLength ^ 2,
                   startColour_ = startColour,
                   endColour_ = endColour}

genConfig :: IO (Either Config GifConfig)
genConfig = do
  putStrLn "Type 'GIF' to generate a GIF. Type anything else for an image."
  simType <- getLine
  if simType == "GIF"
    then Right <$> genGifConfig
    else Left <$> genImageConfig

genGif :: GifConfig -> FilePath -> IO ()
genGif conf filePath = io output
  where io (Left _) = putStrLn "Something unexpected happened when making a GIF"
        io (Right action) = action
        output = writeGifAnimation filePath 5 LoopingForever images
        images = map oneImage conf
        firstConf = head conf
        reps = reps_ firstConf
        oneImage oneConf = sysToGifImage firstConf $ simulate oneConf reps
  
genImage :: Config -> FilePath -> IO()
genImage conf = writeImage conf sys
  where sys = simulate conf reps
        reps = reps_ conf

configToOutput :: Either Config GifConfig -> FilePath -> IO ()
configToOutput (Left conf) fp =  genImage conf fp
configToOutput (Right conf) fp =  genGif conf fp

main :: IO ()
main = do
  config <- genConfig
  putStrLn "Define the output file name"
  filePath <- getLine
  configToOutput config filePath
