-- simple light/wave interaction generator
module Main where

import Codec.Picture (generateImage, writePng, Image, Pixel, Pixel8)

type P2 = (Int, Int)
type P3f = (Float, Float, Float)
type Light = Float
type Sources = [P3f]
type Amplitude = Float
type Wavelength = Float
type Color = Int
type PGM = String

width = 4000
height = 4000
wavelength :: Wavelength
wavelength = 2 *1
ampl :: Amplitude
ampl = 80
doTrunc = True
sources :: Sources
sources = multiply 1 $ move (90,100,0) sourceslily
sourcesa = [(400,400,10),(400,400,100),(400,400,1000)]
sourcesr = [(400,500,10),(400,100,10)]
sourcesg = [(573.21,400,10),(226.79,200,10)]
sourcesb = [(573.21,200,10),(226.79,400,10)]
sources0 = [ (400, 500, 100) --vespa
           , (1600, 500, 100)
           , (1000, 900, 100)
           ]
sources1 = [ (30,150,0), (170,150,0), (30,320,0),(170,320,0)]
sources2 = [ (70,150,0), (130,150,0) ]
sources3 = [ (52,59,0), (160,60,0), (106,90,0), (105,152,0) ] --triangolo + centro
sources4 = [ (283,183,0),(185,302,0),(371,297,0),(35,403,0),(522,411,0),(252,162,0),(315,161,0),(284,162,0) ]

sourceslily = [(115,225,0),(75,100,0),(15,150,0)]

move (x,y,z) xyzs = (\(x',y',z') -> (x+x',y+y',z+z')) <$> xyzs
multiply n xyzs = (\(x,y,z) -> (n*x,n*y,n*z)) <$> xyzs

-- | interaction between n light sources
interact' :: Amplitude -> Wavelength -> P3f -> [P3f] -> Light
interact' ampl wavelength point = abs . sum . map ((*ampl) . sin . (/wavelength) . dist3D point)

-- | 3D distance
dist3D :: P3f -> P3f -> Float
dist3D (x,y,z) (x',y',z') = sqrt $ ((x-x')**2) + ((y-y')**2) + ((z-z')**2)

trunc x | doTrunc && x > 255 = 255
        | doTrunc && x < 0 = 0
        | otherwise = x

-- | computes the color on a given 2D point
calc :: Sources -> Amplitude -> Wavelength -> Int -> Int -> Pixel8
calc sources a w x y = fromIntegral $ trunc $ floor $ interact' a w (fromIntegral x, fromIntegral y, 0) sources

-- | the image
img :: Sources -> Amplitude -> Wavelength -> Image Pixel8
img sources a w = generateImage (calc sources a w) width height

main :: IO ()
main = writePng "image.png" $ img sources ampl wavelength
