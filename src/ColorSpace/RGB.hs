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
    HSL,
    HSV,
  )
where

import ColorSpace.XYZ
import Data.Bits (Bits (shiftR), shiftL)
import Data.Fixed (mod')
import Data.Maybe (fromMaybe)
import Numeric (readHex)
import Optics.Core (A_Lens, Each (each), Iso', LabelOptic' (..), Lens', iso, lens, maximumOf, minimumOf, over, review, simple, to, view, (%), (%~))
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

-------
-- HSL + HSV
-- The following conversion formulas are from wikipedia

data HSL

data HSV

rgb2hsl :: Color D65 RGB -> Color D65 HSL
rgb2hsl col@(RGB r g b) = Color h sL l
  where
    xmax = fromMaybe 0 $ maximumOf channels col
    xmin = fromMaybe 0 $ minimumOf channels col
    c = xmax - xmin
    h'
      | c == 0 = 0
      | xmax == r = (g - b) / c
      | xmax == g = (b - r) / c + 2
      | xmax == b = (r - g) / c + 4
    h = pi / 3.0 * (mod' h' 6.0)
    l = (xmax + xmin) / 2
    sL
      | l == 0 || l == 1 = 0
      | otherwise = (xmax - l) / (l `min` (1 - l))

rgb2hsv :: Color D65 RGB -> Color D65 HSV
rgb2hsv col@(RGB r g b) = Color h sV v
  where
    xmax = fromMaybe 0 $ maximumOf channels col
    v = xmax
    xmin = fromMaybe 0 $ minimumOf channels col
    c = xmax - xmin
    h'
      | c == 0 = 0
      | xmax == r = (g - b) / c
      | xmax == g = (b - r) / c + 2
      | xmax == b = (r - g) / c + 4
    h = pi / 3.0 * (mod' h' 6.0)
    l = (xmax + xmin) / 2
    sV
      | v == 0 = 0
      | otherwise = c / v

hsl2rgb :: Color D65 HSL -> Color D65 RGB
hsl2rgb (Color h s l) = RGB {r, g, b}
  where
    r = f 0
    g = f 8
    b = f 4
    f n = l - a * (max (-1) $ (k - 3) `min` (9 - k) `min` (9 - k))
      where
        a = s * (min l $ 1 - l)
        k = (n + (h / (pi / 6))) `mod'` 12

hsv2rgb :: Color D65 HSV -> Color D65 RGB
hsv2rgb (Color h s v) = RGB {r, g, b}
  where
    r = f 5
    g = f 3
    b = f 1
    f n = v - v * s * (0 `max` (k `min` 1 `min` (4 - k)))
      where
        k = (n + (h / (pi / 3))) `mod'` 6

instance ColorSpace HSL D65 where
  xyz = (iso hsl2rgb rgb2hsl) % (xyz @RGB)

instance ColorSpace HSV D65 where
  xyz = (iso hsv2rgb rgb2hsv) % (xyz @RGB)

pattern HSV ::
  Double ->
  Double ->
  Double ->
  Color D65 HSV
pattern HSV {h, s, v} = Color h s v

pattern HSL ::
  Double ->
  Double ->
  Double ->
  Color D65 HSL
pattern HSL {h, s, l} = Color h s l

instance LabelOptic "h" A_Lens (Color D65 HSL) (Color D65 HSL) Double Double where
  labelOptic :: Lens' (Color D65 HSL) Double
  labelOptic = lens (\(HSL h _ _) -> h) (\(HSL _ s l) h -> HSL h s l)

instance LabelOptic "s" A_Lens (Color D65 HSL) (Color D65 HSL) Double Double where
  labelOptic :: Lens' (Color D65 HSL) Double
  labelOptic = lens (\(HSL _ s _) -> s) (\(HSL h _ l) s -> HSL h s l)

instance LabelOptic "l" A_Lens (Color D65 HSL) (Color D65 HSL) Double Double where
  labelOptic :: Lens' (Color D65 HSL) Double
  labelOptic = lens (\(HSL _ _ l) -> l) (\(HSL h s _) l -> HSL h s l)

instance LabelOptic "h" A_Lens (Color D65 HSV) (Color D65 HSV) Double Double where
  labelOptic :: Lens' (Color D65 HSV) Double
  labelOptic = lens (\(HSV h _ _) -> h) (\(HSV _ s v) h -> HSV h s v)

instance LabelOptic "s" A_Lens (Color D65 HSV) (Color D65 HSV) Double Double where
  labelOptic :: Lens' (Color D65 HSV) Double
  labelOptic = lens (\(HSV _ s _) -> s) (\(HSV h _ v) s -> HSV h s v)

instance LabelOptic "l" A_Lens (Color D65 HSV) (Color D65 HSV) Double Double where
  labelOptic :: Lens' (Color D65 HSV) Double
  labelOptic = lens (\(HSV _ _ v) -> v) (\(HSV h s _) v -> HSV h s v)
