{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module ColorSpace.RGB.HSV
  ( HSV,
    pattern HSV,
    hsv,
  )
where

import ColorSpace.RGB.Space
import ColorSpace.XYZ
import Data.Functor.Rep
import Data.Maybe (fromMaybe)
import Optics.Core

-------
-- HSL + HSV
-- The following conversion formulas are from wikipedia

data HSV (rgb :: *)

rgb2hsv ::
  (RGBSpace rgb, Floating a, Ord a) =>
  Color (RGB rgb) a ->
  Color (HSV rgb) a
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
  Color (HSV rgb) a ->
  Color (RGB rgb) a
hsv2rgb (Color h s v) = RGB r g b
  where
    r = f 5
    g = f 3
    b = f 1
    f n = v - v * s * (0 `max` (k `min` 1 `min` (4 - k)))
      where
        k = mod' (n + (h / (pi / 3))) 6

instance (RGBSpace rgb) => ColorSpace (HSV rgb) where
  type Il (HSV rgb) = IlRGB rgb
  xyz = (iso hsv2rgb rgb2hsv) % (xyz @(RGB rgb))

hsv ::
  forall rgb csp a il.
  ( RGBSpace rgb,
    ColorSpace csp,
    Il csp ~ il,
    IlRGB rgb ~ il,
    Floating a,
    Ord a
  ) =>
  Iso' (Color csp a) (Color (HSV rgb) a)
hsv = xyz % re xyz

pattern HSV ::
  RGBSpace rgb =>
  a ->
  a ->
  a ->
  Color (HSV rgb) a
pattern HSV {h, s, v} = Color h s v

data ChannelHSV = H | S | V
  deriving (Show, Eq)

instance RGBSpace rgb => Representable (Color (HSV rgb)) where
  type Rep (Color (HSV rgb)) = ChannelHSV

  index (HSV h _ _) H = h
  index (HSV _ s _) S = s
  index (HSV _ _ v) V = v

  tabulate f = Color (f H) (f S) (f V)

instance (RGBSpace rgb) => LabelOptic "h" A_Lens (Color (HSV rgb) a) (Color (HSV rgb) a) a a where
  labelOptic :: Lens' (Color (HSV rgb) a) a
  labelOptic = lens (\(HSV h _ _) -> h) (\(HSV _ s v) h -> HSV h s v)

instance (RGBSpace rgb) => LabelOptic "d" A_Lens (Color (HSV rgb) a) (Color (HSV rgb) a) a a where
  labelOptic :: Lens' (Color (HSV rgb) a) a
  labelOptic = lens (\(HSV _ s _) -> s) (\(HSV h _ v) s -> HSV h s v)

instance (RGBSpace rgb) => LabelOptic "v" A_Lens (Color (HSV rgb) a) (Color (HSV rgb) a) a a where
  labelOptic :: Lens' (Color (HSV rgb) a) a
  labelOptic = lens (\(HSV _ _ v) -> v) (\(HSV h s _) v -> HSV h s v)
