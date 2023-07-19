{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module ColorSpace.XYZ
  ( pattern XYZ,
    XYZ,
    Illuminant (..),
    ColorSpace (..),
    Color (..),
    D75,
    D65,
    D55,
    D50,
  )
where

import GHC.Generics (Generic)
import Optics.Core (A_Lens, LabelOptic (..), LabelOptic' (..), review)
import Optics.Getter
import Optics.Iso
import Optics.Lens
import Optics.Optic (NoIx)

data Color il csp = Color Double Double Double

data XYZ

class Illuminant il where
  refWhite :: Color il XYZ

-- Reference illuminants from http://www.brucelindbloom.com
-- he says "all come from ASTM E308-01 except B which comes from Wyszecki & Stiles, p. 769"
data D65 = D65

instance Illuminant D65 where
  refWhite = XYZ {x = 0.95047, y = 1.00000, z = 1.08883}

data D55 = D55

instance Illuminant D55 where
  refWhite = XYZ {x = 0.95682, y = 1.00000, z = 0.92149}

data D50 = D50

instance Illuminant D50 where
  refWhite = XYZ {x = 0.96422, y = 1.00000, z = 0.82521}

data D75 = D75

instance Illuminant D75 where
  refWhite = XYZ {x = 0.94972, y = 1.00000, z = 1.22638}

class ColorSpace csp where
  xyz :: Illuminant il => Iso' (Color il csp) (Color il XYZ)

instance ColorSpace XYZ where
  xyz = iso id id

{-# COMPLETE XYZ #-}

pattern XYZ ::
  (ColorSpace csp, Illuminant il) =>
  Double ->
  Double ->
  Double ->
  Color il csp
pattern XYZ {x, y, z} <-
  (view xyz -> Color x y z)
  where
    XYZ x y z = review xyz (Color x y z :: Color il XYZ)

instance Illuminant il => LabelOptic "x" A_Lens (Color il XYZ) (Color il XYZ) Double Double where
  labelOptic :: Lens' (Color il XYZ) Double
  labelOptic = lens (\(Color x _ _) -> x) (\(Color _ y z) x -> Color x y z)

instance Illuminant il => LabelOptic "y" A_Lens (Color il XYZ) (Color il XYZ) Double Double where
  labelOptic :: Lens' (Color il XYZ) Double
  labelOptic = lens (\(Color _ y _) -> y) (\(Color x _ z) y -> Color x y z)

instance Illuminant il => LabelOptic "z" A_Lens (Color il XYZ) (Color il XYZ) Double Double where
  labelOptic :: Lens' (Color il XYZ) Double
  labelOptic = lens (\(Color _ _ z) -> z) (\(Color x y _) z -> Color x y z)
