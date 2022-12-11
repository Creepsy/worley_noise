module Main where

import qualified Graphics.Image as Img
import qualified Data.Vector as Vec
import qualified System.Random as Rnd
import Data.Vector ((!))
import Data.List (sortBy)
import Data.Function (on)
import Debug.Trace (trace)
import Data.List.Split (chunksOf)

import qualified WorleyNoise as WN

type Position = (Int, Int)
type Seed = Int

main :: IO ()
main = do

    let octaves = [
                (WN.HeightMapInfo 853456 100, 0.6),
                (WN.HeightMapInfo 345345 50, 0.2),
                (WN.HeightMapInfo 23442 40, 0.05),
                (WN.HeightMapInfo 365346 25, 0.05),
                (WN.HeightMapInfo 87823 10, 0.05),
                (WN.HeightMapInfo 65436 5, 0.04),
                (WN.HeightMapInfo 25345645 2, 0.01)]
        img = Img.makeImage (1000, 1000) (Img.PixelY . octaveNoise octaves) :: Img.Image Img.VU Img.Y Double
    

    Img.writeImage "heightmap.png" img

octaveNoise :: [(WN.HeightMapInfo, Double)] -> WN.Position -> Double
octaveNoise octaves pos = foldl (\acc (oct, amplitude) -> acc  + amplitude * WN.worleyNoise oct pos) 0 octaves