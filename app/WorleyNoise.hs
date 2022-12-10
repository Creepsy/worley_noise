module WorleyNoise (
    Seed,
    Position,
    generateWorleyNoise
) where

import GHC.Generics (Generic)
import Data.Bits ((.&.), (.|.), Bits (shift, xor, shiftR, shiftL))
import Debug.Trace (trace)
import Data.Int (Int16)
import qualified Data.Vector as Vec

type Seed = Int
type Position = (Int16, Int16)

generateWorleyNoise :: Seed -> Vec.Vector Double
generateWorleyNoise seed = Vec.generate (2000 * 2000) (\v -> generateFeaturePoint seed (fromIntegral v `mod` 2000, fromIntegral v `div` 2000))

generateFeaturePoint :: Seed -> Position -> Double
generateFeaturePoint seed chunkPos =  fromIntegral (chunkHash .&. 0xff) / 0xff 
    where chunkHash = hash2D seed chunkPos

hash2D :: Seed -> Position -> Int
hash2D seed (xPos, yPos) = hash
    where posNum = fromIntegral xPos .|. (fromIntegral yPos :: Int) `shiftL` 16
          toHash = seed `xor` posNum
          intermediate = (toHash `xor` 61) `xor` (toHash `shiftR` 16) 
          intermediate' = intermediate + (intermediate `shiftL` 3)
          intermediate'' = intermediate' `xor` (intermediate' `shiftR` 4)
          intermediate''' = intermediate'' * 0x27d4eb2d
          hash = intermediate''' `xor` (intermediate''' `shiftR` 15)