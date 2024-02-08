{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
    Color',
    D75,
    D65,
    D55,
    D50,
    chromAdapt,
    chromIso,
    apca,
    channels,
    xy,
  )
where

import Data.Distributive (Distributive (..))
import Data.Distributive.Generic (genericCollect)
import Data.Functor.Rep
import GHC.Generics (Generic, Generic1)
import Optics.At.Core
import Optics.Core (A_Lens, Each (..), LabelOptic (..), LabelOptic' (..), Traversal, Traversal', review, sumOf, traversalVL, (%~), (^.))
import Optics.Getter
import Optics.Iso
import Optics.Lens
import Optics.Optic (NoIx, (%))

type Color :: * -> * -> *
data Color csp a
  = Color
      !a
      !a
      !a
  deriving (Show, Eq, Ord, Generic1, Functor)

instance Distributive (Color csp) where collect = genericCollect

type Color' = Color

-- | Access the channels of a Color.
-- Note that this will cast between color spaces.
-- Use carefully.
channels ::
  forall csp csp' a.
  Traversal (Color csp a) (Color csp' a) a a
channels = traversalVL go
  where
    go f (Color a b c) = Color <$> (f a) <*> (f b) <*> (f c)

type XYZ :: * -> *
data XYZ il

class Illuminant il where
  refWhite :: Fractional a => Color (XYZ il) a

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

class Illuminant (Il csp) => ColorSpace csp where
  type Il csp :: *
  xyz :: (Ord a, Floating a) => Iso' (Color csp a) (Color (XYZ (Il csp)) a)

instance Illuminant il => ColorSpace (XYZ il) where
  type Il (XYZ il) = il
  xyz = simple

data ChannelXYZ = X | Y | Z
  deriving (Eq, Ord, Show, Generic)

instance (ColorSpace (XYZ il)) => Representable (Color (XYZ il)) where
  type Rep (Color (XYZ il)) = ChannelXYZ

  index (Color x _ _) X = x
  index (Color _ y _) Y = y
  index (Color _ _ z) Z = z

  tabulate f = Color (f X) (f Y) (f Z)

-- type instance Index (Color il XYZ a) = Rep (Color il XYZ)
-- type instance IxValue (Color il ch a) = a
-- instance Ixed (Color il csp a) where

{-# COMPLETE XYZ #-}

pattern XYZ ::
  (ColorSpace (XYZ il)) =>
  a ->
  a ->
  a ->
  Color (XYZ il) a
pattern XYZ {x, y, z} = Color x y z

instance Illuminant il => LabelOptic "x" A_Lens (Color (XYZ il) a) (Color (XYZ il) a) a a where
  labelOptic :: Lens' (Color (XYZ il) a) a
  labelOptic = lens (\(Color x _ _) -> x) (\(Color _ y z) x -> Color x y z)

instance Illuminant il => LabelOptic "y" A_Lens (Color (XYZ il) a) (Color (XYZ il) a) a a where
  labelOptic :: Lens' (Color (XYZ il) a) a
  labelOptic = lens (\(Color _ y _) -> y) (\(Color x _ z) y -> Color x y z)

instance Illuminant il => LabelOptic "z" A_Lens (Color (XYZ il) a) (Color (XYZ il) a) a a where
  labelOptic :: Lens' (Color (XYZ il) a) a
  labelOptic = lens (\(Color _ _ z) -> z) (\(Color x y _) z -> Color x y z)

-------
-- chromaticity

-- | xy Chromaticity
-- | Haskell's capitalization rules make thiis a little confusing,
-- | but these are the lowercase xy chromaticity coodinates
xy :: forall il a. (Fractional a, ColorSpace (XYZ il)) => Lens' (Color (XYZ il) a) (a, a)
xy = lens get set
  where
    -- let d = sumOf channels color in (color ^~)
    get :: Color (XYZ il) a -> (a, a)
    get (XYZ {x, y, z}) = let d = x + y + z in (x / d, y / d)
    set :: Color (XYZ il) a -> (a, a) -> Color (XYZ il) a
    set (XYZ {y}) (x', y') =
      XYZ
        { x = y / y' * x',
          y = y,
          z = y / y' * (1 - x' - y')
        }

--------
-- Chromatic Adaptation
-- based on http://www.brucelindbloom.com/index.html?Eqn_ChromAdapt.html
-- I will use Bradford Scaling

-- I am not sure this is the best way to write these rules
{-# RULES "identity chromatic adaptation D65" chromAdapt @D65 @D65 = id #-}

{-# RULES "identity chromatic adaptation D50" chromAdapt @D50 @D50 = id #-}

{-# RULES "identity chromatic adaptation D55" chromAdapt @D55 @D55 = id #-}

{-# RULES "identity chromatic adaptation D75" chromAdapt @D75 @D75 = id #-}

{-# INLINE [2] chromAdapt #-}

-- | Chromatic adaptation between illuminants using Bradford scaling
chromAdapt :: forall i1 i2 a. (Illuminant i1, Illuminant i2, Fractional a) => Color (XYZ i1) a -> Color (XYZ i2) a
chromAdapt = bradfordConeResponseInv . scale . bradfordConeResponse
  where
    (rho_s, gamma_s, beta_s) = bradfordConeResponse $ refWhite @i1
    (rho_d, gamma_d, beta_d) = bradfordConeResponse $ refWhite @i2
    scale (rho, gamma, beta) =
      ( rho_s / rho_d * rho,
        gamma_s / gamma_d * gamma,
        beta_s / beta_d * beta
      )

chromIso :: forall i1 i2 a. (Illuminant i1, Illuminant i2, Fractional a) => Iso' (Color (XYZ i1) a) (Color (XYZ i2) a)
chromIso = iso (chromAdapt) (chromAdapt)

bradfordConeResponse :: (Illuminant il, Fractional a) => Color (XYZ il) a -> (a, a, a)
bradfordConeResponse (XYZ x y z) = (rho, gamma, beta)
  where
    rho = 0.8951000 * x + 0.2664000 * y - 0.1614000 * z
    gamma = -0.7502000 * x + 1.7135000 * y + 0.0367000 * z
    beta = 0.0389000 * x - 0.0685000 * y + 1.0296000 * z

bradfordConeResponseInv :: (Illuminant il, Fractional a) => (a, a, a) -> Color (XYZ il) a
bradfordConeResponseInv (rho, gamma, beta) = (XYZ x y z)
  where
    x = 0.9869929 * rho - 0.1470543 * gamma + 0.1599627 * beta
    y = 0.9869929 * rho - 0.1470543 * gamma + 0.1599627 * beta
    z = 0.9869929 * rho - 0.1470543 * gamma + 0.1599627 * beta

-- | APCA W3 color contrast
-- | Measure contrast between foreground and background colors to ensure readability. Note that the first argument is the text color, the second
-- | is background color.
-- | Taken from https://www.myndex.com/APCA/, refer to that for details.
apca :: (ColorSpace csp, Il csp ~ D65, Floating a, Ord a) => Color csp a -> Color csp a -> a
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
    r_tx = 0.62
    r_bg = 0.65
    -- clamps and scaler
    b_clip = 1.414
    b_thrsh = 0.022
    w_scale = 1.14
    w_off = 0.027
    w_clamp = 0.1
