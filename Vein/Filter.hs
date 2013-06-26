{-# LANGUAGE Rank2Types #-}
module Vein.Filter where

import Control.Vein
import Linear
import Debug.Trace
import Foreign.C.Types

type Wave = V2 CFloat

data FilterParam = FilterParam
    { _filterCutOff :: CFloat
    , _filterQ :: CFloat
    }

data BiquadParam = BiquadParam CFloat CFloat CFloat CFloat CFloat CFloat

biQuadHelper :: (CFloat -> CFloat -> BiquadParam) -> FilterParam -> BiquadParam
biQuadHelper f (FilterParam freq q) = let omega = 2 * pi * freq / 44100 in f omega (sin omega / q / 2) 

biQuadFilter :: Vein m (Wave, BiquadParam) Wave
biQuadFilter = go 0 0 0 0 where
    go x' x'' y' y'' = Vein $ \(x, BiquadParam a0 a1 a2 b0 b1 b2) cont -> do
        let y = (b0 *^ x + b1 *^ x' + b2 *^ x'' - a1 *^ y' - a2 *^ y'') ^* (1/a0)
        cont y $ go x x' y y' 

lowPass :: FilterParam -> BiquadParam
lowPass = biQuadHelper $ \w a -> let v = 1 - cos w in BiquadParam (1 + a) (-2 * cos w) (1 - a) (v / 2) v (v / 2)

highPass :: FilterParam -> BiquadParam
highPass = biQuadHelper $ \w a -> let v = 1 + cos w in BiquadParam (1 + a) (-2 * cos w) (1 - a) (v / 2) v (v / 2)

bandPass :: FilterParam -> BiquadParam
bandPass = biQuadHelper $ \w a -> BiquadParam (1 + a) (-2 * cos w) (1 - a) (sin w / 2) 0 (sin w / 2)

notch :: FilterParam -> BiquadParam
notch = biQuadHelper $ \w a -> BiquadParam (1 + a) (-2 * cos w) (1 - a) 1 (-2 * cos w) 1

allPass :: FilterParam -> BiquadParam
allPass = biQuadHelper $ \w a -> BiquadParam (1 + a) (-2 * cos w) (1 - a) (1 - a) (-2 * cos w) (1 + a)

peaking :: CFloat -> FilterParam -> BiquadParam
peaking gain = biQuadHelper $ \w a -> BiquadParam (1 + a * g) (-2 * cos w) (1 - a * g)
    (1 + a * g) (-2 * cos w) (1 - a * g) where g = sqrt $ 10 ** (gain / 20)
