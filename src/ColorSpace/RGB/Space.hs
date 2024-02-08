{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module ColorSpace.RGB.Space
  ( RGB,
    LinRGB,
    pattern RGB,
    pattern LinRGB,
    rgb,
    linrgb,
    RGBSpec (..),
    RGBSpace (..),
    srgbCompandFwd,
    srgbCompandInv,
    gammaCompandFwd,
    gammaCompandInv,
  )
where

import ColorSpace.XYZ
import Data.Data (Proxy (..))
import Math.Lowlin.Classes
import Math.Lowlin.Types
import Optics.Core

data RGBSpec a = RGBSpec
  { rChrom :: (a, a),
    gChrom :: (a, a),
    bChrom :: (a, a),
    compandFwd :: a -> a,
    compandInv :: a -> a
  }

data RGB rgb

data LinRGB rgb

class Illuminant (IlRGB rgb) => RGBSpace rgb where
  type IlRGB rgb :: *
  rgbSpec :: (Ord a, Floating a) => Proxy rgb -> RGBSpec a
  {-# MINIMAL rgbSpec #-}
  xyz2linRGB :: (Ord a, Floating a) => Iso' (Color (XYZ (IlRGB rgb)) a) (Color (LinRGB rgb) a)
  xyz2linRGB = re linRGB2xyzDefault
  compand :: (Floating a, Ord a) => Iso' (Color (LinRGB rgb) a) (Color (RGB rgb) a)
  compand = compandDefault

matrices :: forall il a. (Ord a, Floating a, Illuminant il) => Proxy il -> RGBSpec a -> (M33 a, M33 a)
matrices _ (RGBSpec {rChrom, gChrom, bChrom}) = (to, from)
  where
    XYZ xw yw zw = refWhite @il
    (xr, yr) = rChrom
    (xg, yg) = gChrom
    (xb, yb) = bChrom
    (xR, yR, zR) = (xr / yr, 1.0 :: a, (1 - xr - yr) / yr)
    (xG, yG, zG) = (xg / yg, 1.0 :: a, (1 - xg - yg) / yg)
    (xB, yB, zB) = (xb / yb, 1.0 :: a, (1 - xb - yb) / yb)
    m = ((xR, xG, xB), (yR, yG, yB), (zR, zG, zB))
    (sr, sg, sb) = inv m |.\ (xw, yw, zw)
    to =
      ( (sr * xR, sg * xG, sb * xB),
        (sr * yR, sg * yG, sb * yB),
        (sr * zR, sg * zG, sb * zB)
      )
    from = inv to

linRGB2xyzDefault :: forall rgb a. (RGBSpace rgb, Ord a, Floating a) => Iso' (Color (LinRGB rgb) a) (Color (XYZ (IlRGB rgb)) a)
linRGB2xyzDefault = iso undefined undefined
  where
    (mTo, mFrom) = matrices (Proxy @(IlRGB rgb)) $ rgbSpec (Proxy @rgb)
    to :: Color (LinRGB rgb) a -> Color (XYZ (IlRGB rgb)) a
    to (Color r g b) = let (x, y, z) = mTo |.\ (r, g, b) in XYZ x y z

    from :: Color (XYZ (IlRGB rgb)) a -> Color (IlRGB rgb) a
    from (XYZ x y z) = let (r, g, b) = mFrom |.\ (x, y, z) in Color r g b

compandDefault ::
  forall rgb a.
  (RGBSpace rgb, Floating a, Ord a) =>
  Iso' (Color (LinRGB rgb) a) (Color (RGB rgb) a)
compandDefault = iso (channels %~ compandFwd) (channels %~ compandInv)
  where
    RGBSpec {compandFwd, compandInv} = rgbSpec (Proxy @rgb)

instance (RGBSpace rgb) => ColorSpace (LinRGB rgb) where
  type Il (LinRGB rgb) = IlRGB rgb
  xyz = re xyz2linRGB

instance (RGBSpace rgb) => ColorSpace (RGB rgb) where
  type Il (RGB rgb) = IlRGB rgb
  xyz = re compand % xyz

pattern RGB ::
  RGBSpace rgb =>
  a ->
  a ->
  a ->
  Color (RGB rgb) a
pattern RGB {r, g, b} = Color r g b

pattern LinRGB ::
  RGBSpace rgb =>
  a ->
  a ->
  a ->
  Color (LinRGB rgb) a
pattern LinRGB {r, g, b} = Color r g b

rgb ::
  forall rgb csp a.
  (ColorSpace csp, Il csp ~ IlRGB rgb, RGBSpace rgb, Ord a, Floating a) =>
  Iso' (Color csp a) (Color (RGB rgb) a)
rgb = xyz % re xyz

linrgb ::
  forall rgb csp a.
  (ColorSpace csp, Il csp ~ IlRGB rgb, RGBSpace rgb, Ord a, Floating a) =>
  Iso' (Color csp a) (Color (LinRGB rgb) a)
linrgb = xyz % xyz2linRGB

-------
-- Common companding functions

-- srgbCompandFwd :: Double -> Double
srgbCompandFwd c
  | c <= 0.0031308 = 12.92 * c
  | otherwise = 1.055 * (c ** 0.41667) - 0.055

-- srgbCompandInv :: Double -> Double
srgbCompandInv c
  | c <= 0.04045 = c / 12.92
  | otherwise = ((c + 0.055) / 1.05) ** 2.4

-- l'CompandFwd :: Double -> Double
l'CompandFwd v
  | v <= eps = v * kappa / 100
  | otherwise = 1.16 * v ** (1 / 3) - 0.16
  where
    eps = 216 / 24389
    kappa = 24389 / 27

-- l'CompandInv :: Double -> Double
l'CompandInv v
  | v <= 0.08 = 100 * v / kappa
  | otherwise = ((v + 0.16) / 1.16) ^ 3
  where
    kappa = 24389 / 27

-- gammaCompandFwd :: Double -> (Double -> Double)
gammaCompandFwd gamma = \v -> v ** (1 / gamma)

-- gammaCompandInv :: Double -> (Double -> Double)
gammaCompandInv gamma = \v -> v ** gamma
