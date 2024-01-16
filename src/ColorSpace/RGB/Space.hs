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

data RGBSpec = RGBSpec
  { rChrom :: (Double, Double),
    gChrom :: (Double, Double),
    bChrom :: (Double, Double),
    compandFwd :: Double -> Double,
    compandInv :: Double -> Double
  }

data RGB rgb

data LinRGB rgb

class Illuminant (Il rgb) => RGBSpace rgb where
  type Il rgb :: *
  rgbSpec :: Proxy rgb -> RGBSpec
  {-# MINIMAL rgbSpec #-}
  xyz2linRGB :: Iso' (Color (Il rgb) XYZ) (Color (Il rgb) (LinRGB rgb))
  xyz2linRGB = re linRGB2xyzDefault
  compand :: Iso' (Color (Il rgb) (LinRGB rgb)) (Color (Il rgb) (RGB rgb))
  compand = compandDefault

matrices :: forall il. Illuminant il => Proxy il -> RGBSpec -> (M33 Double, M33 Double)
matrices _ (RGBSpec {rChrom, gChrom, bChrom}) = (to, from)
  where
    XYZ xw yw zw = refWhite @il
    (xr, yr) = rChrom
    (xg, yg) = gChrom
    (xb, yb) = bChrom
    (xR, yR, zR) = (xr / yr, 1.0 :: Double, (1 - xr - yr) / yr)
    (xG, yG, zG) = (xg / yg, 1.0 :: Double, (1 - xg - yg) / yg)
    (xB, yB, zB) = (xb / yb, 1.0 :: Double, (1 - xb - yb) / yb)
    m = ((xR, xG, xB), (yR, yG, yB), (zR, zG, zB))
    (sr, sg, sb) = inv m |.\ (xw, yw, zw)
    to =
      ( (sr * xR, sg * xG, sb * xB),
        (sr * yR, sg * yG, sb * yB),
        (sr * zR, sg * zG, sb * zB)
      )
    from = inv to

linRGB2xyzDefault :: forall rgb. (RGBSpace rgb) => Iso' (Color (Il rgb) (LinRGB rgb)) (Color (Il rgb) XYZ)
linRGB2xyzDefault = iso undefined undefined
  where
    (mTo, mFrom) = matrices (Proxy @(Il rgb)) $ rgbSpec (Proxy @rgb)
    to :: Color (Il rgb) (LinRGB rgb) -> Color (Il rgb) XYZ
    to (Color r g b) = let (x, y, z) = mTo |.\ (r, g, b) in XYZ x y z

    from :: Color (Il rgb) XYZ -> Color (Il rgb) (LinRGB rgb)
    from (XYZ x y z) = let (r, g, b) = mFrom |.\ (x, y, z) in Color r g b

compandDefault ::
  forall rgb.
  RGBSpace rgb =>
  Iso' (Color (Il rgb) (LinRGB rgb)) (Color (Il rgb) (RGB rgb))
compandDefault = iso (channels %~ compandFwd) (channels %~ compandInv)
  where
    RGBSpec {compandFwd, compandInv} = rgbSpec (Proxy @rgb)

instance (il ~ Il rgb, RGBSpace rgb) => ColorSpace (LinRGB rgb) il where
  xyz = re xyz2linRGB

instance (il ~ Il rgb, RGBSpace rgb) => ColorSpace (RGB rgb) il where
  xyz = re compand % xyz

pattern RGB ::
  RGBSpace rgb =>
  Double ->
  Double ->
  Double ->
  Color (Il rgb) (RGB rgb)
pattern RGB {r, g, b} = Color r g b

pattern LinRGB ::
  RGBSpace rgb =>
  Double ->
  Double ->
  Double ->
  Color (Il rgb) (LinRGB rgb)
pattern LinRGB {r, g, b} = Color r g b

rgb ::
  forall rgb csp.
  (ColorSpace csp (Il rgb), RGBSpace rgb) =>
  Iso' (Color (Il rgb) csp) (Color (Il rgb) (RGB rgb))
rgb = xyz % re xyz

linrgb ::
  forall rgb csp.
  (ColorSpace csp (Il rgb), RGBSpace rgb) =>
  Iso' (Color (Il rgb) csp) (Color (Il rgb) (LinRGB rgb))
linrgb = xyz % xyz2linRGB

-------
-- Common companding functions

srgbCompandFwd :: Double -> Double
srgbCompandFwd c
  | c <= 0.0031308 = 12.92 * c
  | otherwise = 1.055 * (c ** 0.41667) - 0.055

srgbCompandInv :: Double -> Double
srgbCompandInv c
  | c <= 0.04045 = c / 12.92
  | otherwise = ((c + 0.055) / 1.05) ** 2.4

l'CompandFwd :: Double -> Double
l'CompandFwd v
  | v <= eps = v * kappa / 100
  | otherwise = 1.16 * v ** (1 / 3) - 0.16
  where
    eps = 216 / 24389
    kappa = 24389 / 27

l'CompandInv :: Double -> Double
l'CompandInv v
  | v <= 0.08 = 100 * v / kappa
  | otherwise = ((v + 0.16) / 1.16) ^ 3
  where
    kappa = 24389 / 27

gammaCompandFwd :: Double -> (Double -> Double)
gammaCompandFwd gamma = \v -> v ** (1 / gamma)

gammaCompandInv :: Double -> (Double -> Double)
gammaCompandInv gamma = \v -> v ** gamma
