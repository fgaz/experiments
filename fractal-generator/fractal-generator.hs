import Data.Monoid
import Codec.Picture
import Codec.Picture.Types

----
-- some linear algebra

data Trans = Trans Double Double Double Double

-- a c
-- b d

data Vect = Vect Double Double

instance Monoid Trans where
  mempty = Trans 1 0 0 1
  mappend (Trans a b c d) (Trans e f g h) = Trans (a*e + c*f) (b*e + d*f) (a*g + c*h) (b*g + d*h)

instance Monoid Vect where
  mempty = Vect 0 0
  mappend (Vect a b) (Vect c d) = Vect (a+c) (b+d)

applyTrans (Trans a b c d) (Vect x y) = Vect (a*x + c*y) (b*x + d*y)

scale x = Trans x 0 0 x

rotate θ = Trans (cos θ) (sin θ) (-sin θ) (cos θ)

distance (Vect a b) (Vect c d) = sqrt ((a-c)**2 + (b-d)**2)

type Point = (Double, Double)

----

--some fractals
sierpinski = [(scale 0.5, Vect 0 1), (scale 0.5, Vect (cos (-pi/6)) (-0.5)), (scale 0.5, Vect (cos (pi-pi/6)) (-0.5))]
sierpinski2 = [(scale 0.5, Vect (-0.5) 0.5), (scale 0.5, Vect 0.5 0.5), (scale 0.5, Vect 0.5 (-0.5))]
fern = [(Trans 0 0 0 0.16, Vect 0 0), (Trans 0.85 (-0.04) 0.04 0.85, Vect 0 1.60), (Trans 0.20 0.23 (-0.26) 0.22, Vect 0 1.60), (Trans (-0.15) 0.26 0.28 0.24, Vect 0 0.44)]
koch = [(scale (1/3), Vect (-0.67) 0), (scale (1/3), Vect 0.67 0), (scale (1/3) <> rotate (pi/3), Vect (-0.17) 0.29), (scale (1/3) <> rotate (-pi/3), Vect 0.17 0.29)]
cesaro = [(scale 0.46, Vect (-0.27) 0), (scale 0.46, Vect 0.27 0), (scale 0.46 <> rotate (pi*17/36), Vect (-0.02) 0.23), (scale 0.46 <> rotate (-pi*17/36), Vect 0.02 0.23)]
levy = [(rotate (-pi/4) <> scale (sqrt 2 / 2), Vect (-0.25) 0), (rotate (pi/4) <> scale (sqrt 2 / 2), Vect 0.25 0)]
dragon = [(rotate (-pi/4) <> scale (sqrt 2 / 2), Vect (-0.25) 0), (rotate (pi*5/4) <> scale (sqrt 2 / 2), Vect 0.25 0)]
cantorDust = (\x -> (scale (1/3), x)) <$> [ Vect (-2/3) ( 2/3), Vect (2/3) ( 2/3)
                                          , Vect (-2/3) (-2/3), Vect (2/3) (-2/3) ]
tsquare = (\x -> (scale (1/2), x)) <$> [ Vect (-1/2) ( 1/2), Vect (1/2) ( 1/2) -- yeah... this is completely white.
                                       , Vect (-1/2) (-1/2), Vect (1/2) (-1/2) ]
tsquareBoundary = [ (scale (1/2), Vect (-1/2) ( 1/2)), (scale (1/2) <> rotate (-pi/2),Vect (1/2) ( 1/2)) -- unsurprisingly white too
                  , (scale (1/2) <> rotate (pi/2), Vect (-1/2) (-1/2)), (scale (1/2) <> rotate pi, Vect (1/2) (-1/2)) ]
tsquareBoundary3 = [ (scale (1/2), Vect (-1/2) ( 1/2)), (scale (1/2) <> rotate (-pi/2),Vect (1/2) ( 1/2)) -- not white!
                   , (scale (1/2) <> rotate (pi/2), Vect (-1/2) (-1/2)) ]
tsquareVariation1 = [ (scale (1/2), Vect (-1/2) ( 1/2)), (scale (1/2) <> rotate (-pi/2),Vect (1/2) ( 1/2))
                    , (scale (1/2) <> rotate (pi/2), Vect (-1/2) (-1/2)), (scale (1/3), Vect (2/3) (-2/3)) ]
tsquareVariation2 = [ (scale (1/2), Vect (-1/2) ( 1/2)), (scale (1/2) <> rotate (-pi/2),Vect (1/2) ( 1/2))
                    , (scale (1/2) <> rotate (pi/2), Vect (-1/2) (-1/2)), (scale (1/3), Vect (1/3) (-1/3)) ]
tsquareVariation3 = [ (scale (1/2), Vect (-1/2) ( 1/2)), (scale (1/2) <> rotate (-pi/2),Vect (1/2) ( 1/2))
                    , (scale (1/2) <> rotate (pi/2), Vect (-1/2) (-1/2)), (scale (1/3), Vect (sqrt 2) (-sqrt 2)) ]
tsquareVariation4 = [ (scale (1/2), Vect (-1/2) ( 1/2)), (scale (1/2) <> rotate (-pi/2),Vect (1/2) ( 1/2))
                    , (scale (1/2) <> rotate (pi/2), Vect (-1/2) (-1/2)), (scale (1/3), Vect (1/6) (-1/6)) ]
tsquareVariation5 = [ (scale (1/2), Vect (-1/2) ( 1/2)), (scale (1/2) <> rotate (-pi/2),Vect (1/2) ( 1/2))
                    , (scale (1/2) <> rotate (pi/2), Vect (-1/2) (-1/2)), (scale (1/3), Vect (1/128) (-1/128)) ]
tsquareVariation6 = [ (scale (1/2), Vect (-1/2) ( 1/2)), (scale (1/2) <> rotate (-pi/2),Vect (1/2) ( 1/2))
                    , (scale (1/2) <> rotate (pi/2), Vect (-1/2) (-1/2)), (scale (2/3), Vect (-1/3) (1/3)) ]
tsquareVariation7 = [ (scale (1/2), Vect (-1/2) ( 1/2)), (scale (1/2) <> rotate (-pi/2),Vect (1/2) ( 1/2))
                    , (scale (1/2) <> rotate (pi/2), Vect (-1/2) (-1/2)), (scale (2/3) <> rotate pi, Vect (-1/3) (1/3)) ]
tsquareVariation8 = [ (scale (1/2), Vect (-1/2) ( 1/2)), (scale (1/2) <> rotate (-pi/2),Vect (1/2) ( 1/2))
                    , (scale (1/2) <> rotate (pi/2), Vect (-1/2) (-1/2)), (scale (1/3) <> rotate pi, Vect (-1/3) (1/3)) ]
tree90 = [(scale 0.5 <> rotate (-pi/4), Vect (-0.5) 0.5), (scale 0.5 <> rotate (pi/4), Vect 0.5 0.5)]
tree60 = undefined
carpet = (\x -> (scale (1/3), x)) <$> [ Vect (-2/3) ( 2/3), Vect  0  ( 2/3), Vect (2/3) ( 2/3)
                                      , Vect (-2/3)     0 ,                  Vect (2/3)     0
                                      , Vect (-2/3) (-2/3), Vect  0  (-2/3), Vect (2/3) (-2/3) ]
pythagora = undefined


minDist = 0.0001

fract' :: [(Trans, Vect)] -> (Trans, Vect) -> [Point]
fract' generators t =  foldMap (f t) generators
  where f (t,v) (t0,v0) | dist < minDist = pure $ (\(Vect x y) -> (x,y)) v'
                        | otherwise = fract' generators (t',v')
          where v' = v <> applyTrans t v0
                t' = t0 <> t
                dist = distance v v'

fract :: [(Trans, Vect)] -> [Point]
fract generators = fract' generators mempty



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

