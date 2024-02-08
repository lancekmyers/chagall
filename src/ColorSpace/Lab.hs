{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
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
import Data.Functor.Rep
import Optics.Core (A_Lens, Iso', Lens', iso, lens, simple, view, (%))
import Optics.Label (LabelOptic (..))
import Optics.Re (re)

type Lab :: * -> *
data Lab il

instance Illuminant il => ColorSpace (Lab il) where
  type Il (Lab il) = il
  xyz = iso labToXYZ xyzToLab

{-# RULES "lab iso identity on lab D65" lab @_ @(Lab D65) = simple #-}

{-# RULES "lab iso identity on lab D50" lab @_ @(Lab D50) = simple #-}

{-# RULES "lab iso identity on lab D55" lab @_ @(Lab D55) = simple #-}

{-# RULES "lab iso identity on lab D75" lab @_ @(Lab D75) = simple #-}

{-# INLINE [1] lab #-}
lab :: forall a csp il. (Floating a, Ord a, ColorSpace csp, il ~ Il csp) => Iso' (Color csp a) (Color (Lab il) a)
lab = xyz % (re xyz)

pattern Lab ::
  ColorSpace csp =>
  a ->
  a ->
  a ->
  Color csp a
pattern Lab {l, a, b} = Color l a b

data ChannelLab = L | A | B
  deriving (Show, Eq)

instance ColorSpace (Lab il) => Representable (Color (Lab il)) where
  type Rep (Color (Lab il)) = ChannelLab

  index (Lab l _ _) L = l
  index (Lab _ a _) A = a
  index (Lab _ _ b) B = b

  tabulate f = Color (f L) (f A) (f B)

instance Illuminant il => LabelOptic "l" A_Lens (Color (Lab il) a) (Color (Lab il) a) a a where
  labelOptic :: Lens' (Color (Lab il) a) a
  labelOptic = lens (\(Color l _ _) -> l) (\(Color _ a b) l -> Color l a b)

instance Illuminant il => LabelOptic "a" A_Lens (Color (Lab il) a) (Color (Lab il) a) a a where
  labelOptic :: Lens' (Color (Lab il) a) a
  labelOptic = lens (\(Color _ a _) -> a) (\(Color l _ b) a -> Color l a b)

instance Illuminant il => LabelOptic "b" A_Lens (Color (Lab il) a) (Color (Lab il) a) a a where
  labelOptic :: Lens' (Color (Lab il) a) a
  labelOptic = lens (\(Color _ _ b) -> b) (\(Color l a _) b -> Color l a b)

xyzToLab :: forall il a. (Illuminant il, Floating a, Ord a) => Color (XYZ il) a -> Color (Lab il) a
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

labToXYZ :: forall il a. (Illuminant il, Floating a, Ord a) => Color (Lab il) a -> Color (XYZ il) a
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
type LCHab il = Cyl (Lab il)

instance Illuminant il => CylCsp (Lab il)

lchab :: (Floating a, Ord a, ColorSpace csp, Il csp ~ il) => Iso' (Color csp a) (Color (LCHab il) a)
lchab = lab % cyl

pattern LCHab ::
  ColorSpace (LCHab il) =>
  a ->
  a ->
  a ->
  Color (LCHab il) a
pattern LCHab {l, c, h} = Color l c h

------
-- Color difference
-- based on http://www.brucelindbloom.com/index.html?Eqn_Spect_to_XYZ.html

-- | Color difference Delta E (CIE 1976)
deltaE76 :: (Illuminant il, Floating a) => Color (Lab il) a -> Color (Lab il) a -> a
deltaE76 (Lab l1 a1 b1) (Lab l2 a2 b2) =
  sqrt $
    (l1 - l2) ^ 2 + (a1 - a2) ^ 2 + (b1 - b2) ^ 2
