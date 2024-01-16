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
import Data.Fixed (mod')
import Data.Maybe (fromMaybe)
import Optics.Core

-------
-- HSL + HSV
-- The following conversion formulas are from wikipedia

data HSV (rgb :: *)

rgb2hsv ::
  RGBSpace rgb =>
  Color (Il rgb) (RGB rgb) ->
  Color (Il rgb) (HSV rgb)
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

hsv2rgb ::
  RGBSpace rgb =>
  Color (Il rgb) (HSV rgb) ->
  Color (Il rgb) (RGB rgb)
hsv2rgb (Color h s v) = RGB r g b
  where
    r = f 5
    g = f 3
    b = f 1
    f n = v - v * s * (0 `max` (k `min` 1 `min` (4 - k)))
      where
        k = (n + (h / (pi / 3))) `mod'` 6

instance (RGBSpace rgb, il ~ Il rgb, Illuminant il) => ColorSpace (HSV rgb) il where
  xyz = (iso hsv2rgb rgb2hsv) % (xyz @(RGB rgb))

hsv ::
  forall rgb csp.
  (RGBSpace rgb, ColorSpace csp (Il rgb)) =>
  Iso' (Color (Il rgb) csp) (Color (Il rgb) (HSV rgb))
hsv = xyz % re xyz

pattern HSV ::
  RGBSpace rgb =>
  Double ->
  Double ->
  Double ->
  Color (Il rgb) (HSV rgb)
pattern HSV {h, s, v} = Color h s v

instance (RGBSpace rgb, il ~ Il rgb) => LabelOptic "h" A_Lens (Color il (HSV rgb)) (Color il (HSV rgb)) Double Double where
  labelOptic :: Lens' (Color il (HSV rgb)) Double
  labelOptic = lens (\(HSV h _ _) -> h) (\(HSV _ s v) h -> HSV h s v)

instance (RGBSpace rgb, il ~ Il rgb) => LabelOptic "d" A_Lens (Color il (HSV rgb)) (Color il (HSV rgb)) Double Double where
  labelOptic :: Lens' (Color il (HSV rgb)) Double
  labelOptic = lens (\(HSV _ s _) -> s) (\(HSV h _ v) s -> HSV h s v)

instance (RGBSpace rgb, il ~ Il rgb) => LabelOptic "v" A_Lens (Color il (HSV rgb)) (Color il (HSV rgb)) Double Double where
  labelOptic :: Lens' (Color il (HSV rgb)) Double
  labelOptic = lens (\(HSV _ _ v) -> v) (\(HSV h s _) v -> HSV h s v)
