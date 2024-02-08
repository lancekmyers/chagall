{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- | sRGB color space (9 times out of 10 this is the rgb space you want)
module ColorSpace.RGB.SRGB (SRGB, srgb) where

import ColorSpace.RGB.Space
import ColorSpace.XYZ
import Data.Bits (Bits (shiftR), shiftL)
import Data.Data (Proxy)
import Data.Fixed (mod')
import Data.Maybe (fromMaybe)
import Math.Lowlin ((|.\))
import Math.Lowlin.Types
import Numeric (readHex)
import Optics.Core (A_Lens, Each (each), Iso', LabelOptic' (..), Lens', iso, lens, maximumOf, minimumOf, over, review, simple, to, view, (%), (%~))
import Optics.Label (LabelOptic (..))
import Optics.Re (re)
import Text.Printf (printf)

data SRGB

instance RGBSpace SRGB where
  type Il SRGB = D65
  rgbSpec _ =
    RGBSpec
      { rChrom = (0.6400, 0.3300),
        gChrom = (0.300, 0.6000),
        bChrom = (0.1500, 0.0600),
        compandFwd = srgbCompandFwd,
        compandInv = srgbCompandInv
      }
  xyz2linRGB = iso toLin fromLin

toLin :: forall a. (Floating a, Ord a) => Color D65 XYZ a -> Color D65 (LinRGB SRGB) a
toLin (XYZ x y z) = Color r g b
  where
    (r, g, b) = mat |.\ (x, y, z)
    mat :: M33 a
    mat =
      ( (3.2404542, -1.5371385, -0.4985314),
        (-0.9692660, 1.8760108, 0.0415560),
        (0.0556434, -0.2040259, 1.0572252)
      )

fromLin :: forall a. (Floating a, Ord a) => Color D65 (LinRGB SRGB) a -> Color D65 XYZ a
fromLin (Color r g b) = XYZ x y z
  where
    (x, y, z) = mat |.\ (r, g, b)
    mat :: M33 a
    mat =
      ( (0.4124564, 0.3575761, 0.1804375),
        (0.2126729, 0.7151522, 0.0721750),
        (0.0193339, 0.1191920, 0.9503041)
      )

srgb ::
  (Ord a, Floating a, ColorSpace csp D65) =>
  Iso' (Color D65 csp a) (Color D65 (RGB SRGB) a)
srgb = rgb
