{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module ColorSpace.Lab
  ( pattern Lab,
    Lab,
    lab,
    pattern LCHab,
    LCHab,
    lchab,
    deltaE76,
  )
where

import ColorSpace.Cylindrical
import ColorSpace.XYZ
import Optics.Core (A_Lens, Iso', Lens', iso, lens, simple, view, (%))
import Optics.Label (LabelOptic (..))
import Optics.Re (re)

data Lab

instance Illuminant il => ColorSpace Lab il where
  xyz = iso labToXYZ xyzToLab

{-# RULES "lab iso identity on lab D65" lab @Lab @D65 = simple #-}

{-# RULES "lab iso identity on lab D50" lab @Lab @D50 = simple #-}

{-# RULES "lab iso identity on lab D55" lab @Lab @D55 = simple #-}

{-# RULES "lab iso identity on lab D75" lab @Lab @D75 = simple #-}

{-# INLINE [1] lab #-}
lab :: ColorSpace csp il => Iso' (Color il csp) (Color il Lab)
lab = xyz % (re xyz)

pattern Lab ::
  ColorSpace csp il =>
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
type LCHab = Cyl Lab

instance Illuminant il => CylCsp Lab il

lchab :: ColorSpace csp il => Iso' (Color il csp) (Color il LCHab)
lchab = lab % cyl

pattern LCHab ::
  ColorSpace LCHab il =>
  Double ->
  Double ->
  Double ->
  Color il LCHab
pattern LCHab {l, c, h} = Color l c h

------
-- Color difference
-- based on http://www.brucelindbloom.com/index.html?Eqn_Spect_to_XYZ.html

-- | Color difference Delta E (CIE 1976)
deltaE76 :: Illuminant il => Color il Lab -> Color il Lab -> Double
deltaE76 (Lab l1 a1 b1) (Lab l2 a2 b2) =
  sqrt $
    (l1 - l2) ^ 2 + (a1 - a2) ^ 2 + (b1 - b2) ^ 2
