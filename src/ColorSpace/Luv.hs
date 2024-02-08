{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
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

import ColorSpace.Cylindrical
import ColorSpace.XYZ
import Data.Functor.Rep
import Optics.Core (A_Lens, Iso', Lens', iso, lens, simple, view, (%))
import Optics.Label (LabelOptic (..))
import Optics.Re (re)

type Luv :: * -> *
data Luv il

instance Illuminant il => ColorSpace (Luv il) where
  type Il (Luv il) = il
  xyz = iso luvToXYZ xyzToLuv

{-# RULES "luv iso identity on luv D65" luv @(Luv D65) = simple #-}

{-# RULES "luv iso identity on luv D50" luv @(Luv D50) = simple #-}

{-# RULES "luv iso identity on luv D55" luv @(Luv D55) = simple #-}

{-# RULES "luv iso identity on luv D75" luv @(Luv D75) = simple #-}

{-# INLINE [1] luv #-}
luv :: forall csp il a. (ColorSpace csp, Il csp ~ il, Floating a, Ord a) => Iso' (Color csp a) (Color (Luv il) a)
luv = xyz % (re xyz)

pattern Luv ::
  a ->
  a ->
  a ->
  Color (Luv il) a
pattern Luv {l, u, v} = Color l u v

data ChannelLuv = L | U | V
  deriving (Show, Eq)

instance ColorSpace (Luv il) => Representable (Color (Luv il)) where
  type Rep (Color (Luv il)) = ChannelLuv

  index (Luv l _ _) L = l
  index (Luv _ u _) U = u
  index (Luv _ _ v) V = v

  tabulate f = Color (f L) (f U) (f V)

instance Illuminant il => LabelOptic "l" A_Lens (Color (Luv il) a) (Color (Luv il) a) a a where
  labelOptic :: Lens' (Color (Luv il) a) a
  labelOptic = lens (\(Color l _ _) -> l) (\(Color _ u v) l -> Color l u v)

instance Illuminant il => LabelOptic "u" A_Lens (Color (Luv il) a) (Color (Luv il) a) a a where
  labelOptic :: Lens' (Color (Luv il) a) a
  labelOptic = lens (\(Color _ u _) -> u) (\(Color l _ v) u -> Color l u v)

instance Illuminant il => LabelOptic "v" A_Lens (Color (Luv il) a) (Color (Luv il) a) a a where
  labelOptic :: Lens' (Color (Luv il) a) a
  labelOptic = lens (\(Color _ _ v) -> v) (\(Color l u _) v -> Color l u v)

xyzToLuv :: forall il a. (Illuminant il, Floating a, Ord a) => Color (XYZ il) a -> Color (Luv il) a
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

luvToXYZ :: forall il a. (Floating a, Ord a, Illuminant il) => Color (Luv il) a -> Color (XYZ il) a
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
type LCHuv il = Cyl (Luv il)

instance Illuminant il => CylCsp (Luv il)

lchuv :: (ColorSpace csp, Il csp ~ il, Floating a, Ord a) => Iso' (Color csp a) (Color (LCHuv il) a)
lchuv = luv % cyl

pattern LCHuv ::
  a ->
  a ->
  a ->
  Color (LCHuv il) a
pattern LCHuv {l, c, h} = Color l c h
