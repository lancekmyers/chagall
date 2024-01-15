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

import ColorSpace.XYZ (Color (..), ColorSpace (..))
import Optics.Core

data Cyl csp

class ColorSpace csp il => CylCsp csp il

cyl :: CylCsp csp il => Iso' (Color il csp) (Color il (Cyl csp))
cyl = iso to from
  where
    from :: Color il (Cyl csp) -> Color il csp
    from col@(Color a c h) = Color a (c * cos h) (c * sin h)
    to :: Color il csp -> Color il (Cyl csp)
    to col@(Color a x y) = Color a (hypot x y) (atan2 x y)

hypot :: Double -> Double -> Double
hypot x y = sqrt $ x ^ 2 + y ^ 2

instance forall csp il. CylCsp csp il => ColorSpace (Cyl csp) il where
  xyz = re cyl % xyz

instance LabelOptic "hue" A_Lens (Color il (Cyl csp)) (Color il (Cyl csp)) Double Double where
  labelOptic = lens (\(Color _ _ hue) -> hue) (\(Color a c _) h -> Color a c h)

instance LabelOptic "chroma" A_Lens (Color il (Cyl csp)) (Color il (Cyl csp)) Double Double where
  labelOptic = lens (\(Color _ chroma _) -> chroma) (\(Color a _ h) chroma -> Color a chroma h)
