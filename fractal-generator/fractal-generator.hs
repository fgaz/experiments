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

fr0 = [(halveTrans, Vect 0 1), (halveTrans, Vect (cos (-pi/6)) (-0.5)), (halveTrans, Vect (cos (pi-pi/6)) 0.5)]

minDist = 0.0001

fract :: (Trans,Vect) -> [Point]
fract fr =  concat $ f fr <$> fr0
  where f (t,v) (t0,v0) | dist < minDist = [ (\(Vect x y) -> (x,y)) v' ]
                        | otherwise = fract (t',v')
          where v' = vectSum v $ applyTrans t' v0
                t' = composeTrans t0 t
                dist = distance v v'

distance (Vect a b) (Vect c d) = sqrt ((a-c)**2 + (b-d)**2)

applyTrans (Trans a b c d) (Vect x y) = Vect (a*x + c*y) (b*x + d*y)

composeTrans (Trans a b c d) (Trans e f g h) = Trans (a*e + c*f) (b*e + d*f) (a*g + c*h) (b*g + d*h)

vectSum (Vect a b) (Vect c d) = Vect (a+c) (b+d)

placepix i (x,y) = writePixel i x' y' (255 :: Pixel8)
  where x' = min 1999 $ max 0 $ floor (x*1000+1000)
        y' = min 1999 $ max 0 $ floor (y*1000+1000)

main = do
  imgm <- createMutableImage 2000 2000 (0 :: Pixel8)
  let pixels = fract (idTrans,idVect)
  --mapM_ print pixels
  mapM_ (placepix imgm) pixels
  img <- freezeImage imgm
  writeBitmap "image.bmp" img

--TODO check that all transformations approach zero

