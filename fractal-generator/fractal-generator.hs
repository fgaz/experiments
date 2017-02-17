import Codec.Picture
import Codec.Picture.Types

data Trans = Trans Double Double Double Double

-- a c
-- b d

data Vect = Vect Double Double

type Point = (Double, Double)

type FractDef = [(Trans,Vect)]

idTrans = Trans 1 0 0 1

halveTrans = Trans 0.5 0 0 0.5

idVect = Vect 0 0

scale x = Trans x 0 0 x

rotate θ = Trans (cos θ) (sin θ) (-sin θ) (cos θ)

sierpinski = [(halveTrans, Vect 0 1), (halveTrans, Vect (cos (-pi/6)) (-0.5)), (halveTrans, Vect (cos (pi-pi/6)) (-0.5))]
fern = [(Trans 0 0 0 0.16, Vect 0 0), (Trans 0.85 (-0.04) 0.04 0.85, Vect 0 1.60), (Trans 0.20 0.23 (-0.26) 0.22, Vect 0 1.60), (Trans (-0.15) 0.26 0.28 0.24, Vect 0 0.44)]

minDist = 0.0001

fract' :: [(Trans, Vect)] -> (Trans, Vect) -> [Point]
fract' generators t =  concat $ f t <$> generators
  where f (t,v) (t0,v0) | dist < minDist = [ (\(Vect x y) -> (x,y)) v' ]
                        | otherwise = fract' generators (t',v')
          where v' = vectSum v $ applyTrans t' v0
                t' = composeTrans t0 t
                dist = distance v v'

fract :: [(Trans, Vect)] -> [Point]
fract generators = fract' generators (idTrans, idVect)

distance (Vect a b) (Vect c d) = sqrt ((a-c)**2 + (b-d)**2)

applyTrans (Trans a b c d) (Vect x y) = Vect (a*x + c*y) (b*x + d*y)

composeTrans (Trans a b c d) (Trans e f g h) = Trans (a*e + c*f) (b*e + d*f) (a*g + c*h) (b*g + d*h)

vectSum (Vect a b) (Vect c d) = Vect (a+c) (b+d)

placepix i (x,y) = writePixel i x' y' (255 :: Pixel8)
  where x' = min 1999 $ max 0 $ floor (x*1000+1000)
        y' = min 1999 $ max 0 $ floor (y*1000+1000)

main = do
  imgm <- createMutableImage 2000 2000 (0 :: Pixel8)
  let pixels = fract sierpinski
  --mapM_ print pixels
  mapM_ (placepix imgm) pixels
  img <- freezeImage imgm
  writeBitmap "image.bmp" img

--TODO check that all transformations approach zero

