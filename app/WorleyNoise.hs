module WorleyNoise (
    Seed,
    Position,
    ChunkSize,
    HeightMapInfo(..),
    worleyNoise,
    index2D,
    shiftPosition
) where

import qualified Data.Vector as Vec

import Data.Bits ((.&.), (.|.), Bits (shift, xor, shiftR, shiftL))
import Data.List (minimumBy)
import Data.Function (on)
import Data.Vector ((!))

type Seed = Int
type Position = (Int, Int)
type ChunkSize = Int
type Size = (Int, Int)

data HeightMapInfo = HeightMapInfo {
    seed :: Seed,
    chunkSize :: ChunkSize
}

-- add option to choose second or third closest point -> seperate for efficency ?

-- normal worley noise

worleyNoise :: HeightMapInfo -> Position -> Double
worleyNoise hmInfo@(HeightMapInfo _ chunkSize) pos = calculateNoiseValue hmInfo closest pos
    where closest = closestFeaturePoint hmInfo pos

closestFeaturePoint :: HeightMapInfo -> Position -> Position
closestFeaturePoint hmInfo pos = minimumBy (compare `on` distanceSquared pos) surroundingFeaturePoints
    where surroundingFeaturePoints = map (generateFeaturePoint hmInfo) . surroundingChunks hmInfo $ pos

-- general functions

calculateNoiseValue :: HeightMapInfo -> Position -> Position -> Double
calculateNoiseValue (HeightMapInfo _ chunkSize) first second = fromIntegral (distanceSquared first second) / fromIntegral (chunkSize ^ 2)

surroundingChunks :: HeightMapInfo -> Position -> [Position]
surroundingChunks hmInfo pos = [(currChunkX + xOff, currChunkY + yOff) | xOff <- [-1..1], yOff <- [-1..1]]
    where (currChunkX, currChunkY) = chunkPosition hmInfo pos

chunkPosition :: HeightMapInfo -> Position -> Position
chunkPosition (HeightMapInfo _ chunkSize) (xPos, yPos) = (xPos `div` chunkSize, yPos `div` chunkSize)

distanceSquared :: Position -> Position -> Int
distanceSquared (firstX, firstY) (secondX, secondY) = deltaX * deltaX + deltaY * deltaY
    where deltaX = firstX - secondX
          deltaY = firstY - secondY

generateFeaturePoint :: HeightMapInfo -> Position -> Position
generateFeaturePoint (HeightMapInfo seed chunkSize) chunkPos@(chunkX, chunkY) = (xPos, yPos) 
    where chunkHash = hash2D seed chunkPos
          deltaX = (chunkHash .&. 0xffff) `mod` chunkSize
          deltaY = ((chunkHash `shiftR` 16) .&. 0xffff) `mod` chunkSize
          xPos = chunkX * chunkSize + deltaX
          yPos = chunkY * chunkSize + deltaY

hash2D :: Seed -> Position -> Int
hash2D seed (xPos, yPos) = hash
    where posNum = xPos .|. yPos `shiftL` 16
          toHash = seed `xor` posNum
          intermediate = (toHash `xor` 61) `xor` (toHash `shiftR` 16) 
          intermediate' = intermediate + (intermediate `shiftL` 3)
          intermediate'' = intermediate' `xor` (intermediate' `shiftR` 4)
          intermediate''' = intermediate'' * 0x27d4eb2d
          hash = intermediate''' `xor` (intermediate''' `shiftR` 15)

index2D :: Size -> Vec.Vector a -> Position -> a
index2D (width, _) toIndex (xPos, yPos) = toIndex ! (yPos * width + xPos)

shiftPosition :: (Int, Int) -> Position -> Position
shiftPosition (deltaX, deltaY) (posX, posY) = (posX + deltaX, posY + deltaY)