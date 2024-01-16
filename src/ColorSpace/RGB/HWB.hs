{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}

-- | HWB space
-- | This is potentally nice for color pickers and more
-- | intuituive than HSV/HSL
module ColorSpace.RGB.HWB
  ( HWB,
    pattern HWB,
    hwb,
  )
where

import ColorSpace.RGB.HSV
import ColorSpace.RGB.Space
import ColorSpace.XYZ
import Data.Fixed (mod')
import Data.Maybe (fromMaybe)
import Optics.Core

-------

-- The following conversion formulas are from wikipedia
data HWB (rgb :: *)

-- data HSV

hsv2hwb :: RGBSpace rgb => Color (Il rgb) (HSV rgb) -> Color (Il rgb) (HWB rgb)
hsv2hwb col@(HSV h s v) = Color h w b
  where
    w = (1 - s) * v
    b = 1 - v

hwb2hsv :: RGBSpace rgb => Color (Il rgb) (HWB rgb) -> Color (Il rgb) (HSV rgb)
hwb2hsv (Color h w b) = HSV h s v
  where
    s = 1 - w / (1 - b)
    v = 1 - b

instance (il ~ Il rgb, Illuminant il, RGBSpace rgb) => ColorSpace (HWB rgb) il where
  xyz = (iso hwb2hsv hsv2hwb) % xyz

hwb ::
  forall rgb csp.
  (RGBSpace rgb, ColorSpace csp (Il rgb)) =>
  Iso' (Color (Il rgb) csp) (Color (Il rgb) (HWB rgb))
hwb = xyz % re xyz

pattern HWB ::
  RGBSpace rgb =>
  Double ->
  Double ->
  Double ->
  Color (Il rgb) (HWB rgb)
pattern HWB {h, w, b} = Color h w b

instance
  (RGBSpace rgb, il ~ Il rgb) =>
  LabelOptic "h" A_Lens (Color il (HWB rgb)) (Color il (HWB rgb)) Double Double
  where
  labelOptic :: Lens' (Color il (HWB rgb)) Double
  labelOptic = lens (\(HWB h _ _) -> h) (\(HWB _ w b) h -> HWB h w b)

instance
  (RGBSpace rgb, il ~ Il rgb) =>
  LabelOptic "w" A_Lens (Color il (HWB rgb)) (Color il (HWB rgb)) Double Double
  where
  labelOptic :: Lens' (Color il (HWB rgb)) Double
  labelOptic = lens (\(HWB _ w _) -> w) (\(HWB h _ b) w -> HWB h w b)

instance
  (RGBSpace rgb, il ~ Il rgb) =>
  LabelOptic "b" A_Lens (Color il (HWB rgb)) (Color il (HWB rgb)) Double Double
  where
  labelOptic :: Lens' (Color il (HWB rgb)) Double
  labelOptic = lens (\(HWB _ _ b) -> b) (\(HWB h w _) b -> HWB h w b)
