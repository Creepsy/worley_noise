module Main where

import qualified Graphics.Image as Img
import qualified Data.Vector as Vec
import qualified System.Random as Rnd
import Data.Vector ((!))
import Data.List (sortBy)
import Data.Function (on)
import Debug.Trace (trace)

import qualified WorleyNoise as WN

type Position = (Int, Int)
type Seed = Int

main :: IO ()
main = do

    -- let heightmap = generateWorleyNoise 634560789 (5, 5) 200
        -- img = Img.makeImage (1000, 1000) (\pos -> Img.PixelY $ index2D (1000, 1000) pos heightmap) :: Img.Image Img.VU Img.Y Double
    -- let heightmap = WN.generateWorleyNoise (WN.WorldInfo 234234 10)
        -- img = Img.makeImage (2000, 2000) (\pos -> Img.PixelY $ index2D (2000, 2000) pos heightmap) :: Img.Image Img.VU Img.Y Double
    let world = WN.WorldInfo 234234 100
        img = Img.makeImage (2000, 2000) (\(x, y) -> Img.PixelY $ WN.worleyNoise world (fromIntegral x, fromIntegral y)) :: Img.Image Img.VU Img.Y Double

    Img.writeImage "heightmap.png" img

index2D :: (Int, Int) -> Position -> Vec.Vector a -> a
index2D (width, _) (xPos, yPos) toIndex = toIndex ! (yPos * width + xPos)

-- slow code, for reference only

-- generateWorleyNoise :: Seed -> (Int, Int) -> Int -> Vec.Vector Double
-- generateWorleyNoise seed size@(width, height) cellSize = Vec.generate (width * height * cellSize ^ 2) (generateValue borderedSize cellSize featurePoints . adaptedPosition)
--         where borderedSize = (width + 2, height + 2) -- neighbours for border cells
--               featurePoints = generateFeaturePoints seed borderedSize cellSize 
--               indexToPosition' = indexToPosition (width * cellSize, height * cellSize)
--               adaptedPosition = (\(xPos, yPos) -> (xPos + cellSize, yPos + cellSize)) . indexToPosition'

-- generateValue :: (Int, Int) -> Int -> Vec.Vector Position -> Position -> Double
-- generateValue size cellSize featurePoints pos@(posX, posY) = (fromIntegral . distanceSquared pos . head $ sortedNeighbours) / (0.25 * fromIntegral cellSize ^ 2)
--     where neighbours = neighbourFeaturePoints size cellSize (cellIndex cellSize pos) featurePoints
--           sortedNeighbours = sortBy (compare `on` distanceSquared pos) neighbours

-- distanceSquared :: Position -> Position -> Int
-- distanceSquared (firstX, firstY) (secondX, secondY) = deltaX ^ 2 + deltaY ^ 2 
--     where deltaX = firstX - secondX
--           deltaY = firstY - secondY

-- neighbourFeaturePoints :: (Int, Int) -> Int -> Position -> Vec.Vector Position -> [Position]
-- neighbourFeaturePoints size cellSize (cellX, cellY) featurePoints = zipWith asGlobalPosition neighbourCells . map (($ featurePoints) . index2D size) $ neighbourCells 
--     where neighbourCells = [(xPos, yPos) | xPos <- [cellX - 1 .. cellX + 1], yPos <- [cellY - 1 .. cellY + 1]]
--           asGlobalPosition (cellX, cellY) (posX, posY) = (cellX * cellSize + posX, cellY * cellSize + posY)

-- generateFeaturePoints :: Seed -> (Int, Int) -> Int -> Vec.Vector Position
-- generateFeaturePoints seed (width, height) cellSize = Vec.fromList $ take (width * height) randomPositions
--     where randomPositions = Rnd.randomRs ((0, 0), (cellSize - 1, cellSize - 1)) (Rnd.mkStdGen seed)

-- indexToPosition :: (Int, Int) -> Int -> Position
-- indexToPosition (width, height) index = (index `mod` width, index `div` width)

-- cellIndex :: Int -> Position -> Position
-- cellIndex cellSize (posX, posY) = (posX `div` cellSize, posY `div` cellSize) 

-- gradient :: (Int, Int) -> Img.Pixel Img.Y Double
-- gradient (xPos, yPos) = Img.PixelY $ fromIntegral (xPos * yPos) / 10000