module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Aff (launchAff, Aff(..))
import Data.Maybe (Maybe(..), maybe')
import Data.Either (Either(..), either)
import Data.List (List(..))
import Config as C
import Types as T
import Network.HTTP.Affjax (affjax, defaultRequest, AJAX)


{-
citysq :: T.CitySearchQ
citysq = T.CitySearchQ { cityName: "ANDO" }

payersq :: T.PayerSearchQ
payersq = T.PayerSearchQ { payerName: "BAY" }

doctorbq :: T.DoctorBoundsQ
doctorbq = T.DoctorBoundsQ { bounds: [40, -90, 45, -80]
                             , payRange: [10000,200000]
                             , limit: 20
                             , excludePayers: [100000000053.0 :: Number]
                             , includePayers: [] }

testBounds :: GMaps.Bounds
testBounds = { sw_lat: 40.0, -82, 45, -80] }

citySearchTest :: ∀ aff. IdToken
               -> Aff (ajax :: AJAX | aff) (Either ErrorMsg (Array T.City))
citySearchTest token = do
  CS.getCities (C.apiUrl <> "city/search") token citysq

payerSearchTest :: ∀ aff. IdToken
               -> Aff (ajax :: AJAX | aff) (Either ErrorMsg (Maybe (Array T.Payer)))
payerSearchTest token = do
  apiPostRequest (C.apiUrl <> "payers/search") token payersq

doctorsBoundsTest :: ∀ aff. IdToken
                  -> Aff (ajax :: AJAX | aff) (Either ErrorMsg (Array T.Doctor))
doctorsBoundsTest token =
  Tabs.getDoctors C.apiUrl token doctorbq

citiesBoundsTest :: ∀ aff. IdToken
                  -> Aff (ajax :: AJAX | aff) (Either ErrorMsg (Array T.City))
citiesBoundsTest token =
  Tabs.getCities C.apiUrl token citybq

print = liftEff <<< log

testSuite token = do
  print $ "Fetching cities..."
  val1 <- citiesBoundsTest token
  print $ show val1
  print $ "Fetching doctors..."
  val2 <- doctorsBoundsTest token
  print $ show val2

-}
