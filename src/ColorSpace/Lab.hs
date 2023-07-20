{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module ColorSpace.Lab
  ( pattern Lab,
    Lab,
    lab,
    pattern LCHab,
    LCHab,
    lchab,
  )
where

import ColorSpace.XYZ
import Optics.Core (A_Lens, Iso', Lens', iso, lens, view, (%))
import Optics.Label (LabelOptic (..))
import Optics.Re (re)

data Lab

instance ColorSpace Lab where
  xyz = iso labToXYZ xyzToLab

lab :: (ColorSpace csp, Illuminant il) => Iso' (Color il csp) (Color il Lab)
lab = xyz % (re xyz)

pattern Lab ::
  (ColorSpace csp, Illuminant il) =>
  Double ->
  Double ->
  Double ->
  Color il csp
pattern Lab {l, a, b} <-
  (view lab -> Color l a b)
  where
    Lab l a b = view (re lab) (Color l a b :: Color il Lab)

instance Illuminant il => LabelOptic "l" A_Lens (Color il Lab) (Color il Lab) Double Double where
  labelOptic :: Lens' (Color il Lab) Double
  labelOptic = lens (\(Color l _ _) -> l) (\(Color _ a b) l -> Color l a b)

instance Illuminant il => LabelOptic "a" A_Lens (Color il Lab) (Color il Lab) Double Double where
  labelOptic :: Lens' (Color il Lab) Double
  labelOptic = lens (\(Color _ a _) -> a) (\(Color l _ b) a -> Color l a b)

instance Illuminant il => LabelOptic "b" A_Lens (Color il Lab) (Color il Lab) Double Double where
  labelOptic :: Lens' (Color il Lab) Double
  labelOptic = lens (\(Color _ _ b) -> b) (\(Color l a _) b -> Color l a b)

xyzToLab :: forall il. Illuminant il => Color il XYZ -> Color il Lab
xyzToLab (XYZ x y z) = Color l a b
  where
    XYZ xr yr zr = refWhite @il
    xr' = x / xr
    yr' = y / yr
    zr' = z / zr
    eps = 216 / 24389
    kappa = 24389 / 27
    fx
      | xr' > eps = xr' ** (1 / 3)
      | otherwise = (kappa * xr' + 16) * 116
    fy
      | yr' > eps = yr' ** (1 / 3)
      | otherwise = (kappa * yr' + 16) * 116
    fz
      | zr' > eps = zr' ** (1 / 3)
      | otherwise = (kappa * zr' + 16) * 116
    l = 116 * fy - 16
    a = 500 * (fx - fy)
    b = 200 * (fy - fz)

labToXYZ :: forall il. Illuminant il => Color il Lab -> Color il XYZ
labToXYZ (Color l a b) = Color (xr' * xr) (yr' * yr) (zr' * zr)
  where
    XYZ xr yr zr = refWhite @il
    eps = 216 / 24389
    kappa = 24389 / 27
    xr'
      | fx ^^ 3 > eps = fx ^^ 3
      | otherwise = (116 * fx - 16) / kappa
    yr'
      | l > kappa * eps = ((l + 16) / 116) ^^ 3
      | otherwise = l / kappa
    zr'
      | fz ^^ 3 > eps = fz ^^ 3
      | otherwise = (116 * fz - 16) / kappa
    fx = a / 500 + fy
    fz = fy - b / 200
    fy = (l + 16) / 116

-- | LCH(ab) color (note that the angle is given in radians, not degrees)
data LCHab

instance ColorSpace LCHab where
  xyz = re lab_lch % (xyz @Lab)

lchab :: (ColorSpace csp, Illuminant il) => Iso' (Color il csp) (Color il LCHab)
lchab = xyz % (re xyz)

pattern LCHab ::
  (ColorSpace csp, Illuminant il) =>
  Double ->
  Double ->
  Double ->
  Color il csp
pattern LCHab {l, c, h} <-
  (view lchab -> Color l c h)
  where
    LCHab l c h = view (re lchab) (Color l c h :: Color il LCHab)

instance Illuminant il => LabelOptic "l" A_Lens (Color il LCHab) (Color il LCHab) Double Double where
  labelOptic :: Lens' (Color il LCHab) Double
  labelOptic = lens (\(Color l _ _) -> l) (\(Color _ c h) l -> Color l c h)

instance Illuminant il => LabelOptic "c" A_Lens (Color il LCHab) (Color il LCHab) Double Double where
  labelOptic :: Lens' (Color il LCHab) Double
  labelOptic = lens (\(Color _ c _) -> c) (\(Color l _ h) c -> Color l c h)

instance Illuminant il => LabelOptic "h" A_Lens (Color il LCHab) (Color il LCHab) Double Double where
  labelOptic :: Lens' (Color il LCHab) Double
  labelOptic = lens (\(Color _ _ h) -> h) (\(Color l c _) h -> Color l c h)

lab_lch :: Illuminant il => Iso' (Color il Lab) (Color il LCHab)
lab_lch = iso labTolchab lchabTolab

labTolchab :: Illuminant il => Color il Lab -> Color il LCHab
labTolchab (Color l a b) = Color l c h
  where
    c = sqrt (a ^ 2 + b ^ 2)
    h = atan2 a b

lchabTolab :: Illuminant il => Color il LCHab -> Color il Lab
lchabTolab (Color l c h) = Color l a b
  where
    a = c * cos h
    b = c * sin h
