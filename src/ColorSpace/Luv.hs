{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module ColorSpace.Luv
  ( pattern Luv,
    Luv,
    luv,
    pattern LCHuv,
    LCHuv,
    lchuv,
  )
where

import ColorSpace.XYZ
import Optics.Core (A_Lens, Iso', Lens', iso, lens, view, (%))
import Optics.Label (LabelOptic (..))
import Optics.Re (re)

data Luv

instance ColorSpace Luv where
  xyz = iso luvToXYZ xyzToLuv

luv :: (ColorSpace csp, Illuminant il) => Iso' (Color il csp) (Color il Luv)
luv = xyz % (re xyz)

pattern Luv ::
  (ColorSpace csp, Illuminant il) =>
  Double ->
  Double ->
  Double ->
  Color il csp
pattern Luv {l, u, v} <-
  (view luv -> Color l u v)
  where
    Luv l u v = view (re luv) (Color l u v :: Color il Luv)

instance Illuminant il => LabelOptic "l" A_Lens (Color il Luv) (Color il Luv) Double Double where
  labelOptic :: Lens' (Color il Luv) Double
  labelOptic = lens (\(Color l _ _) -> l) (\(Color _ u v) l -> Color l u v)

instance Illuminant il => LabelOptic "u" A_Lens (Color il Luv) (Color il Luv) Double Double where
  labelOptic :: Lens' (Color il Luv) Double
  labelOptic = lens (\(Color _ u _) -> u) (\(Color l _ v) u -> Color l u v)

instance Illuminant il => LabelOptic "v" A_Lens (Color il Luv) (Color il Luv) Double Double where
  labelOptic :: Lens' (Color il Luv) Double
  labelOptic = lens (\(Color _ _ v) -> v) (\(Color l u _) v -> Color l u v)

xyzToLuv :: forall il. Illuminant il => Color il XYZ -> Color il Luv
xyzToLuv (XYZ x y z) = Color l u v
  where
    XYZ xr yr zr = refWhite @il
    xr' = x / xr
    yr' = y / yr
    zr' = z / zr
    eps = 216 / 24389
    kappa = 24389 / 27
    u' = 4 * x / (x + 15 * y + 3 * z)
    v' = 9 * y / (x + 15 * y + 3 * z)
    ur' = 4 * xr / (xr + 15 * yr + 3 * zr)
    vr' = 9 * yr / (xr + 15 * yr + 3 * zr)
    l
      | yr' > eps = 116 * (yr' ** (1 / 3)) - 16
      | otherwise = kappa * yr'
    u = 13 * l * (u' - ur')
    v = 13 * l * (v' - vr')

luvToXYZ :: forall il. Illuminant il => Color il Luv -> Color il XYZ
luvToXYZ (Color l u v) = Color x y z
  where
    XYZ xr yr zr = refWhite @il
    eps = 216 / 24389
    kappa = 24389 / 27
    y
      | l > kappa * eps = ((l + 16) / 116) ^^ 3
      | otherwise = l / kappa
    x = (d - b) / (a - c)
    z = x * a + b
    a = 1 / 3 * (52 * l / (u + 13 * l * u0) - 1)
    b = -5 * y
    c = -1 / 3
    d = y * (39 * l / (v + 13 * l * v0) - 5)
    u0 = 4 * xr / (xr + 15 * yr + 3 * zr)
    v0 = 9 * yr / (xr + 15 * yr + 3 * zr)

-- | LCH(uv) color (note that the angle is given in radians, not degrees)
data LCHuv

instance ColorSpace LCHuv where
  xyz = re luv_lch % (xyz @Luv)

lchuv :: (ColorSpace csp, Illuminant il) => Iso' (Color il csp) (Color il LCHuv)
lchuv = xyz % (re xyz)

pattern LCHuv ::
  (ColorSpace csp, Illuminant il) =>
  Double ->
  Double ->
  Double ->
  Color il csp
pattern LCHuv {l, c, h} <-
  (view lchuv -> Color l c h)
  where
    LCHuv l c h = view (re lchuv) (Color l c h :: Color il LCHuv)

instance Illuminant il => LabelOptic "l" A_Lens (Color il LCHuv) (Color il LCHuv) Double Double where
  labelOptic :: Lens' (Color il LCHuv) Double
  labelOptic = lens (\(Color l _ _) -> l) (\(Color _ c h) l -> Color l c h)

instance Illuminant il => LabelOptic "c" A_Lens (Color il LCHuv) (Color il LCHuv) Double Double where
  labelOptic :: Lens' (Color il LCHuv) Double
  labelOptic = lens (\(Color _ c _) -> c) (\(Color l _ h) c -> Color l c h)

instance Illuminant il => LabelOptic "h" A_Lens (Color il LCHuv) (Color il LCHuv) Double Double where
  labelOptic :: Lens' (Color il LCHuv) Double
  labelOptic = lens (\(Color _ _ h) -> h) (\(Color l c _) h -> Color l c h)

luv_lch :: Illuminant il => Iso' (Color il Luv) (Color il LCHuv)
luv_lch = iso luvTolchuv lchuvToluv

luvTolchuv :: Illuminant il => Color il Luv -> Color il LCHuv
luvTolchuv (Color l u v) = Color l u v
  where
    c = sqrt (u ^ 2 + v ^ 2)
    h = atan2 u v

lchuvToluv :: Illuminant il => Color il LCHuv -> Color il Luv
lchuvToluv (Color l c h) = Color l u v
  where
    u = c * cos h
    v = c * sin h
