module Tests
  () where

import Data.Text (Text) -- we want to import only Text from Data.Text.
import Data.Map  (Map (..))
import Data.Set  (Set (..))
import Data.Map.Strict
import Test.HSpec
import TypeFunctions
import Types


describe "MonoidTest" $ do
                        it "#1" $ 1 "shouldBe" 2