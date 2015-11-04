module Test.Arhelk.Russian.Lemma(
    testModule
  ) where 

import Arhelk.Core.Rule
import qualified Arhelk.Lexer as L
import Arhelk.Russian.Lemma
import Data.List as L
import Data.Monoid
import Data.Text as T
import Prelude as P hiding (Word)
import qualified Test.Framework as TF
import Test.Framework.Providers.HUnit
import Test.HUnit
import TextShow 

testModule :: TF.Test
testModule = TF.testGroup "Lemmanization" [
    testCase "Root дерев" $ isSameRoot ["дерево", "деревья", "дерева", "деревьев", "дереве"]
  , testCase "Root красн" $ isSameRoot ["красный", "красная", "красное", "красные", "красных", "красного", "красной", "красному", "красным", "красном"]
  , testCase "Root сказ" $ isSameRoot ["сказать", "подсказать", "пересказать", "сказал", "сказала", "пересказала", "сказало", "подсказало", "досказал"]
  , testCase "Substantive деревьев" $ assertFailure $ unpack $ showt $ runRule $ substantive "деревьев"
  , testCase "Adjective красный" $ assertFailure $ unpack $ showt $ runRule $ adjective "красный"
  , testCase "Verb ковать" $ assertFailure $ unpack $ showt $ runRule $ verb "ковать"
  , testCase "Verb куют" $ assertFailure $ unpack $ showt $ runRule $ verb "куют"
  , testCase "Verb играть" $ assertFailure $ unpack $ showt $ runRule $ verb "играть"
  , testCase "Verb сказал" $ assertFailure $ unpack $ showt $ runRule $ verb "сказал"
  , testCase "Adverb тихо" $ assertFailure $ unpack $ showt $ runRule $ adverb "тихо"
  , testCase "Adverb тише" $ assertFailure $ unpack $ showt $ runRule $ adverb "тише"
  , TF.testGroup "Particle" [
      testCase "бы" $ assertEqual "" (particle $ clause "бы")
        [[mkParticle ["бы"]]]
    , testCase "вряд ли" $ do 
        let res = particle $ clause "вряд ли"
            resStr = unpack $ showt res
        assertBool (resStr <> " contains вряд, ли") $ [mkParticle ["вряд"], mkParticle ["ли"]] `elem` res 
        assertBool (resStr <> " contains вряд ли") $ [mkParticle ["вряд", "ли"]] `elem` res
    , testCase "вряд ли, бы" $ do
        let res = particle $ clause "вряд ли бы"
            resStr = unpack $ showt res
        assertBool (resStr <> " contains вряд, ли, бы") $ [mkParticle ["вряд"], mkParticle ["ли"], mkParticle ["бы"]] `elem` res 
        assertBool (resStr <> " contains вряд ли, бы") $ [mkParticle ["вряд", "ли"], mkParticle ["бы"]] `elem` res
    ]
  , TF.testGroup "MoreSpecialized" [
      testCase "бы(?) vs бы" $ assertBool "" $ not $ moreSpecialized (SemiProcWord $ Left "бы") (SemiProcWord $ Right (MultiWord ["бы"]))
    , testCase "бы vs бы(?)" $ assertBool "" $ moreSpecialized (SemiProcWord $ Right (MultiWord ["бы"])) (SemiProcWord $ Left "бы") 
    , testCase "ли(?) vs ли(?)" $ assertBool "" $ moreSpecialized (SemiProcWord $ Left "ли") (SemiProcWord $ Left "ли")
    , do
      let 
        spec = SemiProcWord $ Right (MultiWord ["бы"])
        res = glueLessSpecialized [SemiProcWord $ Left "бы", spec]
      testCase "glue [бы(?), бы]" $ assertBool (unpack $ showt res) $ [spec] == res
    , do
      let 
        spec = SemiProcWord $ Right (MultiWord ["бы"])
        res = glueLessSpecialized [spec, SemiProcWord $ Left "бы"]
      testCase "glue [бы, бы(?)]" $ assertBool (unpack $ showt res) $ [spec] == res
    , testCase "[вряд(?), ли(?), бы(?)] vs [вряд, ли(?), бы]" $ let 
        a = [SemiProcWord $ Left "вряд", SemiProcWord $ Left "ли", SemiProcWord $ Left "бы"]
        b = [SemiProcWord $ Right (MultiWord ["вряд"]), SemiProcWord $ Left "ли", SemiProcWord $ Right (MultiWord ["бы"])]
        in assertBool "" $ not $ moreSpecialized a b 
    , do
      let 
        a1 = SemiProcWord $ Left "вряд"
        a2 = SemiProcWord $ Right (MultiWord ["вряд"])
        b = SemiProcWord $ Left "ли"
        c1 = SemiProcWord $ Left "бы"
        c2 = SemiProcWord $ Right (MultiWord ["бы"])
        d1 = glueLessSpecialized [[a1, b, c1], [a2, b, c2]]
        d2 = [[a2, b, c2]]
      testCase "glue [[вряд(?), ли(?), бы(?)], [вряд, ли(?), бы]]" $ assertBool (unpack $ showt d1) $ d1 == d2
    , do
      let 
        a1 = SemiProcWord $ Left "вряд"
        a2 = SemiProcWord $ Right (MultiWord ["вряд"])
        b = SemiProcWord $ Left "ли"
        c1 = SemiProcWord $ Left "бы"
        c2 = SemiProcWord $ Right (MultiWord ["бы"])
        d1 = glueLessSpecialized [[a2, b, c2], [a1, b, c1]]
        d2 = [[a2, b, c2]]
      testCase "glue [[вряд, ли(?), бы], [вряд(?), ли(?), бы(?)]]" $ assertBool (unpack $ showt d1) $ d1 == d2

    ]
  ]

-- | Makes sentence clause
clause :: Text -> SentenceClause SemiProcWord 
clause = fmap (\w -> UnknownWord (SemiProcWord $ Left w)) . T.words

-- | Makes multiword particle
mkParticle :: [Text] -> Lemma SemiProcWord
mkParticle ps = GrammarParticle (SemiProcWord $ Right $ MultiWord ps) ParticleProperties

-- | Succedes only when all roots of words are equal
isSameRoot :: [Text] -> Assertion
isSameRoot ws = do
  let roots = extractRoot <$> ws 
  assertBool ("Roots are not equal for [" <> unpack (T.intercalate ", " ws) <> "]") $ allSame roots
  where
  extractRoot :: Text -> Text
  extractRoot w = case lemmanize . (\a -> [[a]]) . UnknownWord $ w of 
    [] -> error $ "No lemmas: " <> unpack w
    [[a]] -> case lemmaWord a of
      OneWord {..} -> wordRoot
      ws@(MultiWord {}) -> showt ws
    as -> error $ "Too many lemmas: " <> unpack w <> " (" <> unpack (showt as) <> ")"
  
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