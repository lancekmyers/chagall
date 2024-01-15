{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- | Implementation based on https://observablehq.com/@jrus/jzazbz
module ColorSpace.JzAzBz
  ( JzAzBz,
    pattern JzAzBz,
    jab,
    JzCzHz,
    pattern JzCzHz,
    jch,
    deltaEz,
  )
where

import ColorSpace.XYZ
import Optics.Core (A_Lens, Iso', Lens', iso, lens, simple, view, (%))
import Optics.Label (LabelOptic (..))
import Optics.Re (re)

data JzAzBz

instance ColorSpace JzAzBz D65 where
  xyz = iso jzazbzToXYZ xyzToJzazbz

xyzToJzazbz :: Color' XYZ -> Color' JzAzBz
xyzToJzazbz (Color x y z) = Color jz az bz
  where
    lp = pq $ 0.674207838 * x + 0.382799340 * y - 0.047570458 * z
    mp = pq $ 0.149284160 * x + 0.739628340 * y + 0.083327300 * z
    sp = pq $ 0.070941080 * x + 0.174768000 * y + 0.670970020 * z
    iz = 0.5 * (lp + mp)
    az = 3.524000 * lp - 4.066708 * mp + 0.542708 * sp
    bz = 0.199076 * lp + 1.096799 * mp - 1.295875 * sp
    jz = (0.44 * iz) / (1 - 0.56 * iz) - 1.6295499532821566e-11
    -- perceptual quantizer
    pq x =
      let x' = (x * 1e-4) ** 0.1593017578125
       in ((0.8359375 + 18.8515625 * x') / (1 + 18.6875 * x')) ** 134.034375

jzazbzToXYZ :: Color' JzAzBz -> Color' XYZ
jzazbzToXYZ (Color jz az bz) = Color x y z
  where
    jz' = jz + 1.6295499532821566e-11
    iz = jz' / (0.44 + 0.56 * jz')
    -- cone responses
    l = pqInv $ iz + 1.386050432715393e-1 * az + 5.804731615611869e-2 * bz
    m = pqInv $ iz - 1.386050432715393e-1 * az - 5.804731615611891e-2 * bz
    s = pqInv $ iz - 9.601924202631895e-2 * az - 8.118918960560390e-1 * bz

    x = 1.661373055774069e+00 * l - 9.145230923250668e-01 * m + 2.313620767186147e-01 * s
    y = -3.250758740427037e-01 * l + 1.571847038366936e+00 * m - 2.182538318672940e-01 * s
    z = -9.098281098284756e-02 * l - 3.127282905230740e-01 * m + 1.522766561305260e+00 * s

    --- inveerse quantizer
    pqInv x =
      let x' = x ** 7.460772656268214e-03
       in ((0.8359375 - x') / (18.6875 * x' - 18.8515625)) ** 6.277394636015326

{-# RULES "jab iso identity on jab" jab @JzAzBz @D65 = simple #-}

{-# INLINE [1] jab #-}
jab :: forall csp il. ColorSpace csp il => Iso' (Color il csp) (Color' JzAzBz)
jab = xyz % chromIso % (re xyz)

pattern JzAzBz ::
  ColorSpace csp il =>
  Double ->
  Double ->
  Double ->
  Color il csp
pattern JzAzBz {j, a, b} <-
  (view jab -> Color j a b)
  where
    JzAzBz j a b = view (re jab) (Color j a b :: Color il JzAzBz)

instance Illuminant il => LabelOptic "jz" A_Lens (Color il JzAzBz) (Color il JzAzBz) Double Double where
  labelOptic :: Lens' (Color il JzAzBz) Double
  labelOptic = lens (\(Color j _ _) -> j) (\(Color _ a b) j -> Color j a b)

instance Illuminant il => LabelOptic "az" A_Lens (Color il JzAzBz) (Color il JzAzBz) Double Double where
  labelOptic :: Lens' (Color il JzAzBz) Double
  labelOptic = lens (\(Color _ a _) -> a) (\(Color j _ b) a -> Color j a b)

instance Illuminant il => LabelOptic "bz" A_Lens (Color il JzAzBz) (Color il JzAzBz) Double Double where
  labelOptic :: Lens' (Color il JzAzBz) Double
  labelOptic = lens (\(Color _ _ b) -> b) (\(Color j a _) b -> Color j a b)

-- | Polar coordinate version of JzAzBz
data JzCzHz

instance ColorSpace JzCzHz D65 where
  xyz = re jzazbz_jzczhz % (xyz @JzAzBz)

jch :: ColorSpace csp il => Iso' (Color il csp) (Color' JzCzHz)
jch = jab % jzazbz_jzczhz

jzazbz_jzczhz :: Illuminant il => Iso' (Color il JzAzBz) (Color il JzCzHz)
jzazbz_jzczhz = iso fwd bwd
  where
    fwd (Color jz az bz) = Color jz (sqrt $ az * az + bz * bz) (atan2 bz az)
    bwd (Color jz cz hz) = Color jz (cz * cos hz) (cz * sin hz)

pattern JzCzHz ::
  ColorSpace csp il =>
  Double ->
  Double ->
  Double ->
  Color il csp
pattern JzCzHz {jz, cz, hz} <-
  (view jch -> Color jz cz hz)
  where
    JzCzHz jz az bz = view (re jab) (Color jz az bz :: Color' JzAzBz)

-- -- | Color difference Delta Ez
deltaEz :: Color' JzCzHz -> Color' JzCzHz -> Double
deltaEz (JzCzHz jz1 cz1 hz1) (JzCzHz jz2 cz2 hz2) = dJ ^ 2 + dC ^ 2 + dH2
  where
    dJ = jz2 - jz1
    dC = cz2 - cz1
    dh = hz2 - hz1
    dH2 = 2 * cz1 * cz2 * (1 - cos dh)
