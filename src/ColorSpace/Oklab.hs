{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- | Based on ths post https://bottosson.github.io/posts/oklab/
module ColorSpace.Oklab
  ( Oklab,
    oklab,
    pattern Oklab,
  )
where

import ColorSpace.XYZ
import Optics.Core (A_Lens, Iso', Lens', each, iso, lens, simple, view, (%), (%~))
import Optics.Label (LabelOptic (..))
import Optics.Re (re)

data Oklab

instance ColorSpace Oklab where
  type Il Oklab = D65
  xyz = lms' % re nonLin % re lms

-- % (channels % each % (iso (** 0.333) (** 3.0))) % (iso m2 m2Inv)

-- oklabToXYZ :: Color D65 Oklab -> Color D65 XYZ
-- oklabToXYZ = _

-- xyzToOklab :: Color D65 XYZ -> Color D65 Oklab
-- xyzToOklab = _

data LMS

data LMS'

xyzToCone :: Floating a => Color' (XYZ D65) a -> Color' LMS a
xyzToCone (Color x y z) = Color l m s
  where
    l = 0.8189330101 * x + 0.3618667424 * y - 0.1288597137 * z
    m = 0.0329845436 * x + 0.9293118715 * y + 0.0361456387 * z
    s = 0.0482003018 * x + 0.2643662691 * y + 0.6338517070 * z

xyzFromCone :: Floating a => Color' LMS a -> Color (XYZ D65) a
xyzFromCone (Color l m s) = Color x y z
  where
    x = 1.22701 * l - 0.5578 * m + 0.281256 * s
    y = -0.0405802 * l + 1.11226 * m - 0.0716767 * s
    z = -0.0763813 * l - 0.421482 * m + 1.58616 * s

instance ColorSpace LMS where
  type Il LMS = D65
  xyz = re lms

lms :: Floating a => Iso' (Color' (XYZ D65) a) (Color' LMS a)
lms = iso xyzToCone xyzFromCone

m2 :: Floating a => Color' LMS' a -> Color' Oklab a
m2 (Color l' m' s') = Color l a b
  where
    l = 0.2104542553 * l' + 0.7936177850 * m' - 0.0040720468 * s'
    a = 1.9779984951 * l' - 2.4285922050 * m' + 0.4505937099 * s'
    b = 0.0259040371 * l' + 0.7827717662 * m' - 0.8086757660 * s'

m2Inv :: Floating a => Color' Oklab a -> Color' LMS' a
m2Inv (Color l a b) = Color l' m' s'
  where
    l' = 1.0 * l + 0.396338 * a + 0.215804 * b
    m' = 1.0 * l - 0.105561 * a - 0.0638542 * b
    s' = 1.0 * l - 0.0894842 * a - 1.29149 * b

lms' :: Floating a => Iso' (Color' Oklab a) (Color' LMS' a)
lms' = iso m2Inv m2

nonLin :: Floating a => Iso' (Color' LMS a) (Color' LMS' a)
nonLin = iso (channels %~ (** 0.3)) (channels %~ (^^ 3))

{-# RULES "oklab iso identity on oklab" oklab @Oklab @D65 = simple #-}

{-# INLINE [1] oklab #-}
oklab :: forall csp a. (Floating a, Ord a, ColorSpace csp, Il csp ~ D65) => Iso' (Color csp a) (Color Oklab a)
oklab = xyz % (re xyz)

pattern Oklab ::
  a ->
  a ->
  a ->
  Color Oklab a
pattern Oklab {l, a, b} = Color l a b

instance LabelOptic "l" A_Lens (Color Oklab a) (Color Oklab a) a a where
  labelOptic :: Lens' (Color Oklab a) a
  labelOptic = lens (\(Color l _ _) -> l) (\(Color _ a b) l -> Color l a b)

instance LabelOptic "a" A_Lens (Color Oklab a) (Color Oklab a) a a where
  labelOptic :: Lens' (Color Oklab a) a
  labelOptic = lens (\(Color _ a _) -> a) (\(Color l _ b) a -> Color l a b)

instance LabelOptic "b" A_Lens (Color Oklab a) (Color Oklab a) a a where
  labelOptic :: Lens' (Color Oklab a) a
  labelOptic = lens (\(Color _ _ b) -> b) (\(Color l a _) b -> Color l a b)
