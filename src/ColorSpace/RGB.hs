{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- | In particular this is sRGB.
module ColorSpace.RGB
  ( RGB,
    RGBLin,
    rgb,
    rgbLin,
    pattern RGB,
    pattern RGBLin,
    srgbToHex,
    srgbFromHex,
  )
where

import ColorSpace.XYZ
import Data.Bits (Bits (shiftR), shiftL)
import Numeric (readHex)
import Optics.Core (A_Lens, Each (each), Iso', Lens', iso, lens, over, review, simple, to, view, (%), (%~))
import Optics.Label (LabelOptic (..))
import Optics.Re (re)
import Text.Printf (printf)

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
    r = 3.2404542 * x - 1.5371385 * y - 0.4985314 * z
    g = -0.9692660 * x + 1.8760108 * y + 0.0415560 * z
    b = 0.0556434 * x - 0.2040259 * y + 1.0572252 * z

pattern RGBLin ::
  ColorSpace csp il =>
  Double ->
  Double ->
  Double ->
  Color il csp
pattern RGBLin {rl, gl, bl} <-
  (view rgbLin -> Color rl gl bl)
  where
    RGBLin r g b = review rgbLin (Color r g b :: Color D65 RGBLin)

-- | sRGB (nonLinear)
data RGB

instance ColorSpace RGB D65 where
  xyz = (re compandIso) % xyz

{-# RULES "rgb iso identity on rgb" rgb @RGB @D65 = simple #-}

{-# INLINE [1] rgb #-}
rgb :: forall csp il. ColorSpace csp il => Iso' (Color il csp) (Color D65 RGB)
rgb = xyz % chromIso % (re xyz)

compandIso :: Iso' (Color D65 RGBLin) (Color D65 RGB)
compandIso = iso compand compandInv

compand :: Color D65 RGBLin -> Color D65 RGB
compand = channels %~ go
  where
    go :: Double -> Double
    go c
      | c <= 0.0031308 = 12.92 * c
      | otherwise = 1.055 * (c ** 0.41667) - 0.055

compandInv :: Color D65 RGB -> Color D65 RGBLin
compandInv = channels %~ go
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

instance LabelOptic "rl" A_Lens (Color D65 RGBLin) (Color D65 RGBLin) Double Double where
  labelOptic :: Lens' (Color D65 RGBLin) Double
  labelOptic = lens (\(Color r _ _) -> r) (\(Color _ g b) r -> Color r g b)

instance LabelOptic "gl" A_Lens (Color D65 RGBLin) (Color D65 RGBLin) Double Double where
  labelOptic :: Lens' (Color D65 RGBLin) Double
  labelOptic = lens (\(Color _ g _) -> g) (\(Color r _ b) g -> Color r g b)

instance LabelOptic "bl" A_Lens (Color D65 RGBLin) (Color D65 RGBLin) Double Double where
  labelOptic :: Lens' (Color D65 RGBLin) Double
  labelOptic = lens (\(Color _ _ b) -> b) (\(Color r g _) b -> Color r g b)

-- >>> srgbToWords (Color 0.2 0.3 0.5 :: Color D65 RGB)
-- (51,76,128)
srgbToWords :: Color D65 RGB -> (Word, Word, Word)
srgbToWords (Color r g b) = (r', g', b')
  where
    r' = round (r * 255)
    g' = round (g * 255)
    b' = round (b * 255)

-- >>> srgbToHex (Color (216 / 255) (46 / 255) (157 / 255) :: Color D65 RGB)
-- "#d82e9d"
srgbToHex :: Color D65 RGB -> String
srgbToHex (srgbToWords -> (r, g, b)) = printf "#%02x%02x%02x" r g b

-- >>> srgbFromHex "#d82e9d"
-- Just (Color 0.8470588235294118 0.1803921568627451 0.615686274509804)
srgbFromHex :: String -> Maybe (Color D65 RGB)
srgbFromHex ('#' : _r' : _r : _g' : _g : _b' : _b : []) =
  Color <$> r <*> g <*> b
  where
    hexDig hd
      | hd <= '9' && hd >= '0' = Just $ fromEnum hd - 48
      | hd <= 'f' && hd >= 'a' = Just $ 10 + fromEnum hd - 97
      | otherwise = Nothing
    hex2Dig d1 d2 = do
      x1 <- hexDig d1
      x2 <- hexDig d2
      return $ shiftL x1 4 + x2
    r = (/ 255.0) . fromIntegral <$> hex2Dig _r' _r
    g = (/ 255.0) . fromIntegral <$> hex2Dig _g' _g
    b = (/ 255.0) . fromIntegral <$> hex2Dig _b' _b
srgbFromHex _ = Nothing
