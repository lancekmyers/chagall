{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}

module ColorSpace.RGB.HSL
  ( HSL,
    pattern HSL,
    hsl,
  )
where

import ColorSpace.RGB.Space
import ColorSpace.XYZ
import Data.Fixed (mod')
import Data.Maybe (fromMaybe)
import Optics.Core

-------
-- HSL + HSV
-- The following conversion formulas are from wikipedia

data HSL (rgb :: *)

-- data HSV

rgb2hsl :: RGBSpace rgb => Color (Il rgb) (RGB rgb) -> Color (Il rgb) (HSL rgb)
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

hsl2rgb :: RGBSpace rgb => Color (Il rgb) (HSL rgb) -> Color (Il rgb) (RGB rgb)
hsl2rgb (Color h s l) = RGB r g b
  where
    r = f 0
    g = f 8
    b = f 4
    f n = l - a * (max (-1) $ (k - 3) `min` (9 - k) `min` (9 - k))
      where
        a = s * (min l $ 1 - l)
        k = (n + (h / (pi / 6))) `mod'` 12

instance (il ~ Il rgb, Illuminant il, RGBSpace rgb) => ColorSpace (HSL rgb) il where
  xyz = (iso hsl2rgb rgb2hsl) % (xyz @(RGB rgb))

hsl ::
  forall rgb csp.
  (RGBSpace rgb, ColorSpace csp (Il rgb)) =>
  Iso' (Color (Il rgb) csp) (Color (Il rgb) (HSL rgb))
hsl = xyz % re xyz

pattern HSL ::
  RGBSpace rgb =>
  Double ->
  Double ->
  Double ->
  Color (Il rgb) (HSL rgb)
pattern HSL {h, s, l} = Color h s l

instance
  (RGBSpace rgb, il ~ Il rgb) =>
  LabelOptic "h" A_Lens (Color il (HSL rgb)) (Color il (HSL rgb)) Double Double
  where
  labelOptic :: Lens' (Color il (HSL rgb)) Double
  labelOptic = lens (\(HSL h _ _) -> h) (\(HSL _ s l) h -> HSL h s l)

instance
  (RGBSpace rgb, il ~ Il rgb) =>
  LabelOptic "s" A_Lens (Color il (HSL rgb)) (Color il (HSL rgb)) Double Double
  where
  labelOptic :: Lens' (Color il (HSL rgb)) Double
  labelOptic = lens (\(HSL _ s _) -> s) (\(HSL h _ l) s -> HSL h s l)

instance
  (RGBSpace rgb, il ~ Il rgb) =>
  LabelOptic "l" A_Lens (Color il (HSL rgb)) (Color il (HSL rgb)) Double Double
  where
  labelOptic :: Lens' (Color il (HSL rgb)) Double
  labelOptic = lens (\(HSL _ _ l) -> l) (\(HSL h s _) l -> HSL h s l)
