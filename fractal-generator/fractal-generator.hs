import Codec.Picture
import Codec.Picture.Types

data Trans = Trans Double Double Double Double

-- a c
-- b d

data Vect = Vect Double Double

type Point = (Double, Double)

v0 = Vect 1 1

ts0 = [Trans 0.5 (-0.5) 0.5 0.5, Trans 0.5 (-0.2) 0.3 0.3]

minDist = 0.0001

fract :: [Trans] -> Vect -> [Point]
fract ts v = foldMap f ts
  where f t | dist < minDist = [ (\(Vect x y) -> (x,y)) v' ]
            | otherwise = fract (flip composeTrans t <$> ts0) v'
          where v' = vectSum v $ applyTrans t v0
                dist = distance v v'

distance (Vect a b) (Vect c d) = sqrt ((a-c)**2 + (b-d)**2)

applyTrans (Trans a b c d) (Vect x y) = Vect (a*x + c*y) (b*x + d*y)

composeTrans (Trans a b c d) (Trans e f g h) = Trans (a*e + c*f) (b*e + d*f) (a*g + c*h) (b*g + d*h)

vectSum (Vect a b) (Vect c d) = Vect (a+c) (b+d)

placepix i (x,y) = writePixel i x' y' (255 :: Pixel8)
  where x' = abs $ floor (x*500)
        y' = abs $ floor (y*500)

main = do
  imgm <- createMutableImage 500 500 (0 :: Pixel8)
  let pixels = fract ts0 v0
  mapM_ (placepix imgm) pixels
  img <- freezeImage imgm
  writeBitmap "image.bmp" img

--TODO check that all transformations approach zero

