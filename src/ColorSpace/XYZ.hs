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
    chromAdapt,
    chromIso,
    apca,
  )
where

import GHC.Generics (Generic)
import Optics.Core (A_Lens, LabelOptic (..), LabelOptic' (..), review, (^.))
import Optics.Getter
import Optics.Iso
import Optics.Lens
import Optics.Optic (NoIx, (%))

data Color il csp
  = Color
      {-# UNPACK #-} !Double
      {-# UNPACK #-} !Double
      {-# UNPACK #-} !Double
  deriving (Show, Eq, Ord)

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

class Illuminant il => ColorSpace csp il where
  xyz :: Iso' (Color il csp) (Color il XYZ)

instance Illuminant il => ColorSpace XYZ il where
  xyz = iso id id

{-# COMPLETE XYZ #-}

pattern XYZ ::
  (ColorSpace csp il, Illuminant il) =>
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

--------
-- Chromatic Adaptation
-- based on http://www.brucelindbloom.com/index.html?Eqn_ChromAdapt.html
-- I will use Bradford Scaling

-- | Chromatic adaptation between illuminants using Bradford scaling
chromAdapt :: forall i1 i2. (Illuminant i1, Illuminant i2) => Color i1 XYZ -> Color i2 XYZ
chromAdapt = bradfordConeResponseInv . scale . bradfordConeResponse
  where
    (rho_s, gamma_s, beta_s) = bradfordConeResponse $ refWhite @i1
    (rho_d, gamma_d, beta_d) = bradfordConeResponse $ refWhite @i2
    scale (rho, gamma, beta) =
      ( rho_s / rho_d * rho,
        gamma_s / gamma_d * gamma,
        beta_s / beta_d * beta
      )

chromIso :: forall i1 i2. (Illuminant i1, Illuminant i2) => Iso' (Color i1 XYZ) (Color i2 XYZ)
chromIso = iso (chromAdapt) (chromAdapt)

bradfordConeResponse :: Illuminant il => Color il XYZ -> (Double, Double, Double)
bradfordConeResponse (XYZ x y z) = (rho, gamma, beta)
  where
    rho = 0.8951000 * x + 0.2664000 * y - 0.1614000 * z
    gamma = -0.7502000 * x + 1.7135000 * y + 0.0367000 * z
    beta = 0.0389000 * x - 0.0685000 * y + 1.0296000 * z

bradfordConeResponseInv :: Illuminant il => (Double, Double, Double) -> Color il XYZ
bradfordConeResponseInv (rho, gamma, beta) = (XYZ x y z)
  where
    x = 0.9869929 * rho - 0.1470543 * gamma + 0.1599627 * beta
    y = 0.9869929 * rho - 0.1470543 * gamma + 0.1599627 * beta
    z = 0.9869929 * rho - 0.1470543 * gamma + 0.1599627 * beta

-- | APCA W3 color contrast
-- | Measure contrast between foreground and background colors to ensure readability. Note that the first argument is the text color, the second
-- | is background color.
-- | Taken from https://www.myndex.com/APCA/, refer to that for details.
apca :: ColorSpace csp D65 => Color D65 csp -> Color D65 csp -> Double
apca tx bg
  | abs s_apc < w_clamp = 0.0
  | s_apc > 0 = 100 * (s_apc - w_off)
  | s_apc < 0 = 100 * (s_apc + w_off)
  where
    s_apc
      | ybg > ytx = w_scale * (ybg ** n_bg - ytx ** n_tx)
      | otherwise = w_scale * (ybg ** r_bg - ytx ** r_tx)
    ytx = tx ^. xyz % #y % to sc
    ybg = bg ^. xyz % #y % to sc
    sc y
      | y < 0 = 0
      | y < b_thrsh = y + (b_thrsh - y) ** b_clip
      | otherwise = y
    -- powercurve
    n_tx = 0.57
    n_bg = 0.56
    r_tx = 0.63
    r_bg = 0.65
    -- clamps and scaler
    b_clip = 1.414
    b_thrsh = 0.022
    w_scale = 1.14
    w_off = 0.027
    w_clamp = 0.1
