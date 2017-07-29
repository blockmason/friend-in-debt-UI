module UI.IconGenerator where

import FriendInDebt.Prelude
import Control.Monad.Eff           (Eff, kind Effect)
import Data.String as S

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

import Network.Eth.FriendInDebt as F

type GradientCss = String

foreign import randomGradientImpl ∷ forall e. Unit → Eff e GradientCss

randomGradient ∷ forall e. Eff e GradientCss
randomGradient = randomGradientImpl unit

-- generateGradientForFriend  ∷ forall p i. F.FoundationId → GradientCss
-- generateGradientForFriend _ = randomGradient

generatedIcon ∷ forall p i. String → GradientCss → H.HTML p i
generatedIcon name gradient =
  let initial = S.take 1 name
  in
    HH.div
      [HP.class_ $ HH.ClassName "generated-icon",
       HP.attr (HH.AttrName "style") gradient]
      [ HH.text initial]

-- type GradientRec = { name ∷ String, colors ∷ Array String}
-- newtype Gradient = Gradient GradientRec
--
-- foreign import randomGradientImpl :: forall e. Unit -> Eff e GradientRec
--
-- randomGradient :: forall e. Eff e Gradient
-- randomGradient = Gradient <$> randomGradientImpl unit

-- foreign import randomGradientWithSeedImpl :: forall e. String -> Eff e GradientRec
--
-- randomGradientWithSeed :: forall e. String -> Eff e Gradient
-- randomGradient seed = Gradient <$> randomGradientImpl seed
