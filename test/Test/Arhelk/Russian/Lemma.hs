module Test.Arhelk.Russian.Lemma(
    testModule
  ) where 

import Arhelk.Russian.Lemma
import Data.List as L
import Data.Monoid
import Data.Text as T
import qualified Test.Framework as TF
import Test.Framework.Providers.HUnit
import Test.HUnit

testModule :: TF.Test
testModule = TF.testGroup "Lemmanization" [
    testCase "Root дерев" $ isSameRoot ["дерево", "деревья", "дерева", "деревьев", "дереве"]
  , testCase "Root красн" $ isSameRoot ["красный", "красная", "красное", "красные", "красных", "красного", "красной", "красному", "красным", "красном"]
  , testCase "Root сказ" $ isSameRoot ["сказать", "подсказать", "пересказать", "сказал", "сказала", "пересказала", "сказало", "подсказало", "досказал"]
  ]

-- | Succedes only when all roots of words are equal
isSameRoot :: [Text] -> Assertion
isSameRoot ws = assertBool ("Roots are not equal for [" <> unpack (T.intercalate ", " ws) <> "]") $ allSame $ lemmaRoot . lemmanize <$> ws

-- | Returns True if all elements in list are equal each other
-- Note: allSame [] == True
allSame :: Eq a => [a] -> Bool
allSame = snd . L.foldl' go (Nothing, True)
  where
    go (_ , False) _ = (Nothing, False) 
    go (Nothing, True) w = (Just w, True)
    go (Just w1, True) w2 
      | w1 == w2 = (Just w1, True)
      | otherwise = (Nothing, False)