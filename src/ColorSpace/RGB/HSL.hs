{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module ColorSpace.RGB.HSL
  ( HSL,
    pattern HSL,
    hsl,
  )
where

import ColorSpace.RGB.Space
import ColorSpace.XYZ
import Data.Maybe (fromMaybe)
import Optics.Core

mod' x c
  | x <= c && x >= 0 = x
  | x < 0 = mod' x c + c
  | x > c = mod' x c - c

-------
-- HSL + HSV
-- The following conversion formulas are from wikipedia

data HSL (rgb :: *)

-- data HSV

rgb2hsl :: (RGBSpace rgb, Floating a, Ord a) => Color (RGB rgb) a -> Color (HSL rgb) a
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

hsl2rgb :: (RGBSpace rgb, Floating a, Ord a) => Color (HSL rgb) a -> Color (RGB rgb) a
hsl2rgb (Color h s l) = RGB r g b
  where
    r = f 0
    g = f 8
    b = f 4
    f n = l - a * (max (-1) $ (k - 3) `min` (9 - k) `min` (9 - k))
      where
        a = s * (min l $ 1 - l)
        k = (n + (h / (pi / 6))) `mod'` 12

instance RGBSpace rgb => ColorSpace (HSL rgb) where
  type Il (HSL rgb) = IlRGB rgb
  xyz = (iso hsl2rgb rgb2hsl) % (xyz @(RGB rgb))

hsl ::
  forall rgb csp a.
  (RGBSpace rgb, IlRGB rgb ~ Il csp, ColorSpace csp, Floating a, Ord a) =>
  Iso' (Color csp a) (Color (HSL rgb) a)
hsl = xyz % re xyz

pattern HSL ::
  RGBSpace rgb =>
  a ->
  a ->
  a ->
  Color (HSL rgb) a
pattern HSL {h, s, l} = Color h s l

instance
  (RGBSpace rgb) =>
  LabelOptic "h" A_Lens (Color (HSL rgb) a) (Color (HSL rgb) a) a a
  where
  labelOptic :: Lens' (Color (HSL rgb) a) a
  labelOptic = lens (\(HSL h _ _) -> h) (\(HSL _ s l) h -> HSL h s l)

instance
  (RGBSpace rgb) =>
  LabelOptic "s" A_Lens (Color (HSL rgb) a) (Color (HSL rgb) a) a a
  where
  labelOptic :: Lens' (Color (HSL rgb) a) a
  labelOptic = lens (\(HSL _ s _) -> s) (\(HSL h _ l) s -> HSL h s l)

instance
  (RGBSpace rgb) =>
  LabelOptic "l" A_Lens (Color (HSL rgb) a) (Color (HSL rgb) a) a a
  where
  labelOptic :: Lens' (Color (HSL rgb) a) a
  labelOptic = lens (\(HSL _ _ l) -> l) (\(HSL h s _) l -> HSL h s l)
