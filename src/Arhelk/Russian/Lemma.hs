module Arhelk.Russian.Lemma(
    Lemma(..)
  , lemmanize
  , substantive
  , genderByDeclension
  , module X
  ) where 

import Arhelk.Russian.Lemma.Data as X
import Arhelk.Russian.Rule
import Control.Monad
import Data.Text as T
import Lens.Simple

-- | Parsed word with detached prefixes, postfixes and determinied speach part
data Lemma = Lemma {
-- | Initial full word
  lemmaSource :: Text
-- | Semanitc root
, lemmaRoot :: Text
-- | Semantic prefixes
, lemmaPrefixes :: [Text]
-- | Semantic postfixes
, lemmaPostfixes :: [Text]
-- | Determinied part of speach
, lemmaSpeachPart :: Maybe SpeachPart
}

-- | Calculate work lemma
lemmanize :: Text -> Lemma
lemmanize = error "unimplemented"

endsWith :: Text -> [Text] -> Bool
endsWith w ws = or $ ends <$> ws
  where
    ends s = takeEnd (T.length s) w == s 

-- | Try to guess quantity, declension and case by ending of word
substantive :: Text -> Rule SubstantiveProperties
substantive w = do
  propose substCase Nominativus $ do 
    propose substQuantity GrammarSingle $ do
      when (w `endsWith` ["а", "я"]) $ imply substDeclension FirstDeclension
      when (w `endsWith` ["", "о", "е"]) $ imply substDeclension SecondDeclension
      when (w `endsWith` [""]) $ imply substDeclension ThirdDeclension
    propose substQuantity GrammarMultiple $ do
      when (w `endsWith` ["ы", "и"]) $ imply substDeclension  FirstDeclension
      when (w `endsWith` ["ы", "и", "а", "я"]) $ imply substDeclension SecondDeclension
      when (w `endsWith` ["и"]) $ imply substDeclension ThirdDeclension

  propose substCase Genitivus $ do 
    propose substQuantity GrammarSingle $ do 
      when (w `endsWith` ["ы", "и"]) $ imply substDeclension FirstDeclension
      when (w `endsWith` ["а", "я"]) $ imply substDeclension SecondDeclension
      when (w `endsWith` ["и"]) $ imply substDeclension ThirdDeclension
    propose substQuantity GrammarMultiple $ do 
      when (w `endsWith` ["", "ов", "ев", "ей"]) implyNothing

  propose substCase Dativus $ do 
    propose substQuantity GrammarSingle $ do 
      when (w `endsWith` ["е", "и"]) $ imply substDeclension FirstDeclension
      when (w `endsWith` ["у", "ю"]) $ imply substDeclension SecondDeclension
      when (w `endsWith` ["и"]) $ imply substDeclension ThirdDeclension
    propose substQuantity GrammarMultiple $ do 
      when (w `endsWith` ["ам", "ям"]) implyNothing

  propose substCase Accusativus $ do 
    propose substQuantity GrammarSingle $ do 
      when (w `endsWith` ["у", "ю"]) $ imply substDeclension FirstDeclension
      when (w `endsWith` ["о", "е"]) $ imply substDeclension SecondDeclension
      when (w `endsWith` [""]) $ imply substDeclension ThirdDeclension
    propose substQuantity  GrammarMultiple $ do 
      when (w `endsWith` ["ами", "ями"]) implyNothing

  propose substCase Praepositionalis $ do 
    propose substQuantity GrammarSingle $ do 
      when (w `endsWith` ["е", "и"]) $ imply substDeclension FirstDeclension
      when (w `endsWith` ["е", "и"]) $ imply substDeclension SecondDeclension
      when (w `endsWith` ["и"]) $ imply substDeclension ThirdDeclension
    propose substQuantity GrammarMultiple $ do 
      when (w `endsWith` ["ах", "ях"]) implyNothing

-- | Guesses gender by declension
genderByDeclension :: Rule SubstantiveProperties -> Rule SubstantiveProperties
genderByDeclension = implyMap $ \s -> case view substDeclension s of 
  Just FirstDeclension -> upd s <$> [GrammarMale, GrammarFemale]
  Just SecondDeclension -> upd s <$> [GrammarMale, GrammarNeuter]
  Just ThirdDeclension -> upd s <$> [GrammarFemale]
  Nothing -> [s]
  where
    upd s v = set substGender (Just v) s