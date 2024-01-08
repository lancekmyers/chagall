{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- | In particular this is sRGB.
module RGB
  ( RGB,
    RGBLin,
    rgb,
    rgbLin,
    pattern RGB,
    pattern RGBLin,
  )
where

import ColorSpace.XYZ
import Optics.Core (A_Lens, Iso', Lens', iso, lens, review, view, (%))
import Optics.Label (LabelOptic (..))
import Optics.Re (re)

-- | Linear sRGB
data RGBLin

instance ColorSpace RGBLin D65 where
  xyz = iso rgbLinToXYZ xyzToRGBLin

rgbLin :: ColorSpace csp il => Iso' (Color il csp) (Color D65 RGBLin)
rgbLin = xyz % chromIso % (re xyz)

rgbLinToXYZ :: Color D65 RGBLin -> Color D65 XYZ
rgbLinToXYZ (Color r g b) = Color x y z
  where
    x = 0.4124564 * r + 0.3575761 * g + 0.1804375 * b
    y = 0.2126729 * r + 0.7151522 * g + 0.0721750 * b
    z = 0.0193339 * r + 0.1191920 * g + 0.9503041 * b

xyzToRGBLin :: Illuminant il => Color il XYZ -> Color il RGBLin
xyzToRGBLin (Color x y z) = Color r g b
  where
    r = 3.2404542 * r - 1.5371385 * g - 0.4985314 * b
    g = -0.9692660 * r + 1.8760108 * g + 0.0415560 * b
    b = 0.0556434 * r - 0.2040259 * g + 1.0572252 * b

pattern RGBLin ::
  ColorSpace csp il =>
  Double ->
  Double ->
  Double ->
  Color il csp
pattern RGBLin {r, g, b} <-
  (view rgbLin -> Color r g b)
  where
    RGBLin r g b = review rgbLin (Color r g b :: Color D65 RGBLin)

-- | sRGB (nonLinear)
data RGB

instance ColorSpace RGB D65 where
  xyz = (re compandIso) % xyz

rgb :: ColorSpace csp il => Iso' (Color il csp) (Color D65 RGB)
rgb = xyz % chromIso % (re xyz)

compandIso :: Iso' (Color D65 RGBLin) (Color D65 RGB)
compandIso = iso compand compandInv

compand :: Color D65 RGBLin -> Color D65 RGB
compand (Color r g b) = Color (go r) (go g) (go b)
  where
    go c
      | c <= 0.0031308 = 12.92 * c
      | otherwise = 1.055 * (c ** 0.41667) - 0.055

compandInv :: Color D65 RGB -> Color D65 RGBLin
compandInv (Color r g b) = Color (go r) (go g) (go b)
  where
    go c
      | c <= 0.04045 = c / 12.92
      | otherwise = ((c + 0.055) / 1.05) ** 2.4

pattern RGB ::
  ColorSpace csp il =>
  Double ->
  Double ->
  Double ->
  Color il csp
pattern RGB {r, g, b} <-
  (view rgb -> Color r g b)
  where
    RGB r g b = review rgb (Color r g b :: Color D65 RGB)

instance LabelOptic "r" A_Lens (Color D65 RGB) (Color D65 RGB) Double Double where
  labelOptic :: Lens' (Color D65 RGB) Double
  labelOptic = lens (\(Color r _ _) -> r) (\(Color _ g b) r -> Color r g b)

instance LabelOptic "g" A_Lens (Color D65 RGB) (Color D65 RGB) Double Double where
  labelOptic :: Lens' (Color D65 RGB) Double
  labelOptic = lens (\(Color _ g _) -> g) (\(Color r _ b) g -> Color r g b)

instance LabelOptic "b" A_Lens (Color D65 RGB) (Color D65 RGB) Double Double where
  labelOptic :: Lens' (Color D65 RGB) Double
  labelOptic = lens (\(Color _ _ b) -> b) (\(Color r g _) b -> Color r g b)
