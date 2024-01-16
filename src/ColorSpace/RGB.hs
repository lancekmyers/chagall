{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- | In particular this is sRGB.
module ColorSpace.RGB
  ( SRGB,
    srgb,
    HSL,
    hsl,
    pattern HSL,
    HSV,
    hsv,
    pattern HSV,
    RGB,
    LinRGB,
    pattern RGB,
    pattern LinRGB,
    rgb,
    linrgb,
    -- io utils
    rgbToHex,
    rgbFromHex,
    rgbToWords,
  )
where

import ColorSpace.RGB.HSL (HSL, hsl, pattern HSL)
import ColorSpace.RGB.HSV (HSV, hsv, pattern HSV)
import ColorSpace.RGB.SRGB (SRGB, srgb)
import ColorSpace.RGB.Space
import ColorSpace.XYZ
import Data.Bits (Bits (shiftL))
import Optics.Core (A_Lens, LabelOptic, lens)
import Optics.Label (LabelOptic (..))
import Text.Printf (printf)

-- >>> rgbToWords (Color 0.2 0.3 0.5 :: Color D65 (RGB SRGB))
-- (51,76,128)
rgbToWords ::
  RGBSpace rgb =>
  Color (Il rgb) (RGB rgb) ->
  (Word, Word, Word)
rgbToWords (Color r g b) = (r', g', b')
  where
    r' = round (r * 255)
    g' = round (g * 255)
    b' = round (b * 255)

-- >>> srgbToHex (Color (216 / 255) (46 / 255) (157 / 255) :: Color D65 RGB)
-- "#d82e9d"
rgbToHex :: RGBSpace rgb => Color (Il rgb) (RGB rgb) -> String
rgbToHex (rgbToWords -> (r, g, b)) = printf "#%02x%02x%02x" r g b

-- >>> srgbFromHex "#d82e9d"
-- Just (Color 0.8470588235294118 0.1803921568627451 0.615686274509804)
rgbFromHex :: String -> Maybe (Color (Il rgb) (RGB rgb))
rgbFromHex ('#' : _r' : _r : _g' : _g : _b' : _b : []) =
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
rgbFromHex _ = Nothing

-------
-- label optics

instance
  (RGBSpace rgb, il ~ Il rgb) =>
  LabelOptic "r" A_Lens (Color il (RGB rgb)) (Color il (RGB rgb)) Double Double
  where
  labelOptic = lens (\(RGB r _ _) -> r) (\(RGB _ g b) r' -> RGB r' g b)

instance
  (RGBSpace rgb, il ~ Il rgb) =>
  LabelOptic "r" A_Lens (Color il (LinRGB rgb)) (Color il (LinRGB rgb)) Double Double
  where
  labelOptic = lens (\(LinRGB r _ _) -> r) (\(LinRGB _ g b) r' -> LinRGB r' g b)

instance
  (RGBSpace rgb, il ~ Il rgb) =>
  LabelOptic "g" A_Lens (Color il (RGB rgb)) (Color il (RGB rgb)) Double Double
  where
  labelOptic = lens (\(RGB _ g _) -> g) (\(RGB r _ b) g' -> RGB r g' b)

instance
  (RGBSpace rgb, il ~ Il rgb) =>
  LabelOptic "g" A_Lens (Color il (LinRGB rgb)) (Color il (LinRGB rgb)) Double Double
  where
  labelOptic = lens (\(LinRGB _ g _) -> g) (\(LinRGB r _ b) g' -> LinRGB r g' b)

instance
  (RGBSpace rgb, il ~ Il rgb) =>
  LabelOptic "b" A_Lens (Color il (RGB rgb)) (Color il (RGB rgb)) Double Double
  where
  labelOptic = lens (\(RGB _ _ b) -> b) (\(RGB r g _) b' -> RGB r g b')

instance
  (RGBSpace rgb, il ~ Il rgb) =>
  LabelOptic "b" A_Lens (Color il (LinRGB rgb)) (Color il (LinRGB rgb)) Double Double
  where
  labelOptic = lens (\(LinRGB _ _ b) -> b) (\(LinRGB r g _) b' -> LinRGB r g b')
