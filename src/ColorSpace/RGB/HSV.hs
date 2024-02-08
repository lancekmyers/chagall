{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}

module ColorSpace.RGB.HSV
  ( HSV,
    pattern HSV,
    hsv,
  )
where

import ColorSpace.RGB.Space
import ColorSpace.XYZ
import Data.Maybe (fromMaybe)
import Optics.Core

-------
-- HSL + HSV
-- The following conversion formulas are from wikipedia

data HSV (rgb :: *)

rgb2hsv ::
  (RGBSpace rgb, Floating a, Ord a) =>
  Color (Il rgb) (RGB rgb) a ->
  Color (Il rgb) (HSV rgb) a
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
    h = pi / 3.0 * mod' h' 6
    l = (xmax + xmin) / 2
    sV
      | v == 0 = 0
      | otherwise = c / v

mod' x c
  | x <= c && x >= 0 = x
  | x < 0 = mod' x c + c
  | x > c = mod' x c - c

hsv2rgb ::
  (RGBSpace rgb, Floating a, Ord a) =>
  Color (Il rgb) (HSV rgb) a ->
  Color (Il rgb) (RGB rgb) a
hsv2rgb (Color h s v) = RGB r g b
  where
    r = f 5
    g = f 3
    b = f 1
    f n = v - v * s * (0 `max` (k `min` 1 `min` (4 - k)))
      where
        k = mod' (n + (h / (pi / 3))) 6

instance (RGBSpace rgb, il ~ Il rgb, Illuminant il) => ColorSpace (HSV rgb) il where
  xyz = (iso hsv2rgb rgb2hsv) % (xyz @(RGB rgb))

hsv ::
  forall rgb csp a.
  (RGBSpace rgb, ColorSpace csp (Il rgb), Floating a, Ord a) =>
  Iso' (Color (Il rgb) csp a) (Color (Il rgb) (HSV rgb) a)
hsv = xyz % re xyz

pattern HSV ::
  RGBSpace rgb =>
  a ->
  a ->
  a ->
  Color (Il rgb) (HSV rgb) a
pattern HSV {h, s, v} = Color h s v

instance (RGBSpace rgb, il ~ Il rgb) => LabelOptic "h" A_Lens (Color il (HSV rgb) a) (Color il (HSV rgb) a) a a where
  labelOptic :: Lens' (Color il (HSV rgb) a) a
  labelOptic = lens (\(HSV h _ _) -> h) (\(HSV _ s v) h -> HSV h s v)

instance (RGBSpace rgb, il ~ Il rgb) => LabelOptic "d" A_Lens (Color il (HSV rgb) a) (Color il (HSV rgb) a) a a where
  labelOptic :: Lens' (Color il (HSV rgb) a) a
  labelOptic = lens (\(HSV _ s _) -> s) (\(HSV h _ v) s -> HSV h s v)

instance (RGBSpace rgb, il ~ Il rgb) => LabelOptic "v" A_Lens (Color il (HSV rgb) a) (Color il (HSV rgb) a) a a where
  labelOptic :: Lens' (Color il (HSV rgb) a) a
  labelOptic = lens (\(HSV _ _ v) -> v) (\(HSV h s _) v -> HSV h s v)
