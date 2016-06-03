-- simple light/wave interaction generator
module Gen where

import Codec.Picture (generateImage, Image, Pixel8)

type P2 = (Int, Int)
type P3f = (Float, Float, Float)
type Light = Float
type Sources = [P3f]
type Amplitude = Float
type Wavelength = Float
type Color = Int
type PGM = String

doTrunc :: Bool
doTrunc = True

-- | interaction between n light sources
interact' :: Amplitude -> Wavelength -> P3f -> [P3f] -> Light
interact' ampl wavelength point = abs . sum . map ((*ampl) . sin . (/wavelength) . dist3D point)

-- | 3D distance
dist3D :: P3f -> P3f -> Float
dist3D (x,y,z) (x',y',z') = sqrt $ ((x-x')**2) + ((y-y')**2) + ((z-z')**2)

trunc :: Integer -> Integer
trunc x | doTrunc && x > 255 = 255
        | doTrunc && x < 0 = 0
        | otherwise = x

-- | computes the color on a given 2D point
calc :: Sources -> Amplitude -> Wavelength -> Int -> Int -> Pixel8
calc sources a w x y = fromIntegral $ trunc $ floor $ interact' a w (fromIntegral x, fromIntegral y, 0) sources

-- | the image
img :: (Int, Int) -> Sources -> Amplitude -> Wavelength -> Image Pixel8
img (width, height) sources a w = generateImage (calc sources a w) width height
