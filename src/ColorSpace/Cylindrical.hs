{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module ColorSpace.Cylindrical
  ( CylCsp,
    Cyl,
    cyl,
  )
where

import ColorSpace.XYZ (Color (..), ColorSpace (..), xy)
import Optics.Core
  ( A_Lens,
    Iso',
    LabelOptic (..),
    ReversibleOptic (re),
    iso,
    lens,
    (%),
  )

data Cyl csp

class ColorSpace csp il => CylCsp csp il

cyl :: forall csp il a. (Floating a, Ord a, CylCsp csp il) => Iso' (Color il csp a) (Color il (Cyl csp) a)
cyl = iso to from
  where
    from :: Color il (Cyl csp) a -> Color il csp a
    from col@(Color a c h) = Color a (c * cos h) (c * sin h)
    to :: Color il csp a -> Color il (Cyl csp) a
    to col@(Color a x y) = Color a (hypot x y) (atan2' x y)

atan2' :: (Ord a, Floating a) => a -> a -> a
atan2' y x
  | x > 0 = atan $ y / x
  | y > 0 = pi / 2 - atan (x / y)
  | y < 0 = -pi / 2 - atan (x / y)
  | x < 0 = atan (y / x) + pi

hypot :: Floating a => a -> a -> a
hypot x y = sqrt $ x ^ 2 + y ^ 2

instance forall csp il. CylCsp csp il => ColorSpace (Cyl csp) il where
  xyz = re cyl % xyz

instance LabelOptic "hue" A_Lens (Color il (Cyl csp) a) (Color il (Cyl csp) a) a a where
  labelOptic = lens (\(Color _ _ hue) -> hue) (\(Color a c _) h -> Color a c h)

instance LabelOptic "chroma" A_Lens (Color il (Cyl csp) a) (Color il (Cyl csp) a) a a where
  labelOptic = lens (\(Color _ chroma _) -> chroma) (\(Color a _ h) chroma -> Color a chroma h)
