module Arhelk.Russian.Lemma(
    testModule
  ) where 

import qualified Test.Framework as TF
import Test.Framework.Providers.HUnit
import Data.Text 
import Test.HUnit

testModule :: [TF.Test]
testModule = [
    testCase "Корень дерев" $ isSameRoot ["дерево", "деревья", "дерева", "деревьев", "дереве"]
  ]

isSameRoot :: [Text] -> Assertion
isSameRoot ws = assertFailure "Unimplemented"