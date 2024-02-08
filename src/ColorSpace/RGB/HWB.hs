{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

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

hsv2hwb :: (RGBSpace rgb, Floating a) => Color (HSV rgb) a -> Color (HWB rgb) a
hsv2hwb col@(HSV h s v) = Color h w b
  where
    w = (1 - s) * v
    b = 1 - v

hwb2hsv :: (RGBSpace rgb, Floating a) => Color (HWB rgb) a -> Color (HSV rgb) a
hwb2hsv (Color h w b) = HSV h s v
  where
    s = 1 - w / (1 - b)
    v = 1 - b

instance RGBSpace rgb => ColorSpace (HWB rgb) where
  type Il (HWB rgb) = IlRGB rgb
  xyz = (iso hwb2hsv hsv2hwb) % xyz

hwb ::
  forall rgb csp a.
  (RGBSpace rgb, ColorSpace csp, Il csp ~ IlRGB rgb, Ord a, Floating a) =>
  Iso' (Color csp a) (Color (HWB rgb) a)
hwb = xyz % re xyz

pattern HWB ::
  RGBSpace rgb =>
  a ->
  a ->
  a ->
  Color (HWB rgb) a
pattern HWB {h, w, b} = Color h w b

instance
  (RGBSpace rgb) =>
  LabelOptic "h" A_Lens (Color (HWB rgb) a) (Color (HWB rgb) a) a a
  where
  labelOptic :: Lens' (Color (HWB rgb) a) a
  labelOptic = lens (\(HWB h _ _) -> h) (\(HWB _ w b) h -> HWB h w b)

instance
  (RGBSpace rgb) =>
  LabelOptic "w" A_Lens (Color (HWB rgb) a) (Color (HWB rgb) a) a a
  where
  labelOptic :: Lens' (Color (HWB rgb) a) a
  labelOptic = lens (\(HWB _ w _) -> w) (\(HWB h _ b) w -> HWB h w b)

instance
  (RGBSpace rgb) =>
  LabelOptic "b" A_Lens (Color (HWB rgb) a) (Color (HWB rgb) a) a a
  where
  labelOptic :: Lens' (Color (HWB rgb) a) a
  labelOptic = lens (\(HWB _ _ b) -> b) (\(HWB h w _) b -> HWB h w b)
