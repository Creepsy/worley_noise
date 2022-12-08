module Main where

import qualified Graphics.Image as Img

main :: IO ()
main = do
    let img = Img.makeImage (100, 100) gradient :: Img.Image Img.VU Img.Y Double

    Img.writeImage "heightmap.png" img

gradient :: (Int, Int) -> Img.Pixel Img.Y Double
gradient (xPos, yPos) = Img.PixelY $ fromIntegral (xPos * yPos) / 10000