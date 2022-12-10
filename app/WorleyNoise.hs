module WorleyNoise (
    Seed,
    Position,
    ChunkSize,
    HeightMapInfo(..),
    worleyNoise
) where

import Data.Bits ((.&.), (.|.), Bits (shift, xor, shiftR, shiftL))
import Data.Int (Int16)
import qualified Data.Vector as Vec
import Data.List (minimumBy)
import Data.Function (on)

type Seed = Int
type Position = (Int16, Int16)
type ChunkSize = Int16

data HeightMapInfo = HeightMapInfo {
    seed :: Seed,
    chunkSize :: ChunkSize
}

-- add implementation for generate many (with grid bounds as parameter)
-- also add option to choose second or third closest point -> seperate for efficency ?
-- maybe also remove casting between int 16 and int32; use int64?!?!

worleyNoise :: HeightMapInfo -> Position -> Double
worleyNoise hmInfo@(HeightMapInfo _ chunkSize) pos = fromIntegral distSquared / fromIntegral (chunkSize ^ 2) -- better way to calculate noise value?
    where closest = closestFeaturePoint hmInfo pos
          distSquared = distanceSquared pos closest

closestFeaturePoint :: HeightMapInfo -> Position -> Position
closestFeaturePoint hmInfo pos = minimumBy (compare `on` distanceSquared pos) featurePoints
    where featurePoints = map (generateFeaturePoint hmInfo) . surroundingChunks hmInfo $ pos

surroundingChunks :: HeightMapInfo -> Position -> [Position]
surroundingChunks hmInfo pos = surroundings
    where (currChunkX, currChunkY) = chunkPosition hmInfo pos
          surroundings = [(currChunkX + xOff, currChunkY + yOff) | xOff <- [-1..1], yOff <- [-1..1]]

chunkPosition :: HeightMapInfo -> Position -> Position
chunkPosition (HeightMapInfo _ chunkSize) (xPos, yPos) = (xPos `div` chunkSize, yPos `div` chunkSize)

distanceSquared :: Position -> Position -> Int
distanceSquared (firstX, firstY) (secondX, secondY) = deltaX ^ 2 + deltaY ^ 2
    where deltaX = fromIntegral $ firstX - secondX
          deltaY = fromIntegral $  firstY - secondY

generateFeaturePoint :: HeightMapInfo -> Position -> Position
generateFeaturePoint (HeightMapInfo seed chunkSize) chunkPos@(chunkX, chunkY) = (xPos, yPos) 
    where chunkHash = hash2D seed chunkPos
          deltaX = fromIntegral (chunkHash .&. 0xffff) `mod` chunkSize
          deltaY = fromIntegral ((chunkHash `shiftR` 16) .&. 0xffff) `mod` chunkSize
          xPos = fromIntegral $ chunkX * chunkSize + deltaX
          yPos = fromIntegral $ chunkY * chunkSize + deltaY

hash2D :: Seed -> Position -> Int
hash2D seed (xPos, yPos) = hash
    where posNum = fromIntegral xPos .|. (fromIntegral yPos :: Int) `shiftL` 16
          toHash = seed `xor` posNum
          intermediate = (toHash `xor` 61) `xor` (toHash `shiftR` 16) 
          intermediate' = intermediate + (intermediate `shiftL` 3)
          intermediate'' = intermediate' `xor` (intermediate' `shiftR` 4)
          intermediate''' = intermediate'' * 0x27d4eb2d
          hash = intermediate''' `xor` (intermediate''' `shiftR` 15)