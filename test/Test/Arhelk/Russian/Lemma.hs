module Test.Arhelk.Russian.Lemma(
    testModule
  ) where 

import qualified Test.Framework as TF
import Test.Framework.Providers.HUnit
import Data.Text 
import Test.HUnit

testModule :: TF.Test
testModule = TF.testGroup "Lemmanization" [
    testCase "Root дерев" $ isSameRoot ["дерево", "деревья", "дерева", "деревьев", "дереве"]
  , testCase "Root красн" $ isSameRoot ["красный", "красная", "красное", "красные", "красных", "красного", "красной", "красному", "красным", "красном"]
  , testCase "Root сказ" $ isSameRoot ["сказать", "подсказать", "пересказать", "сказал", "сказала", "пересказала", "сказало", "подсказало", "досказал"]
  ]

isSameRoot :: [Text] -> Assertion
isSameRoot ws = assertFailure "Unimplemented"