module Arhelk.Russian.Lemma(
    Lemma(..)
  , lemmanize
  , substantive
  , adjective
  , genderByDeclension
  , verb
  , adverb
  , module X
  ) where 

import Arhelk.Russian.Lemma.Data as X
import Arhelk.Core.Rule
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
      when (w `endsWith` ["а", "я", "у", "ю"]) $ imply substDeclension SecondDeclension
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
      when (w `endsWith` ["", "о", "е", "а", "я", "у", "ю"]) $ imply substDeclension SecondDeclension
      when (w `endsWith` [""]) $ imply substDeclension ThirdDeclension
    propose substQuantity  GrammarMultiple $ do 
      when (w `endsWith` ["", "ы", "и", "а", "я", "ов", "ев", "ей"]) implyNothing

  propose substCase Ablativus $ do 
    propose substQuantity GrammarSingle $ do 
      when (w `endsWith` ["ой", "ою", "ей", "ею"]) $ imply substDeclension FirstDeclension
      when (w `endsWith` ["ом", "ем"]) $ imply substDeclension SecondDeclension
      when (w `endsWith` ["ью"]) $ imply substDeclension ThirdDeclension
    propose substQuantity GrammarMultiple $ do 
      when (w `endsWith` ["ами", "ями", "ми"]) implyNothing

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

adjective :: Text -> Rule AdjectiveProperties
adjective w = do 
  propose adjCase Nominativus $ do 
    propose adjQuantity GrammarSingle $ do
      when (w `endsWith` ["ой", "ый", "ий"]) $ imply adjGender GrammarMale
      when (w `endsWith` ["оe", "ee"]) $ imply adjGender GrammarNeuter
      when (w `endsWith` ["ая", "яя"]) $ imply adjGender GrammarFemale
    propose adjQuantity GrammarMultiple $ do
      when (w `endsWith` ["ые", "ие"]) implyNothing

  propose adjCase Genitivus $ do 
    propose adjQuantity GrammarSingle $ do
      when (w `endsWith` ["ого", "eго"]) $ do 
        imply adjGender GrammarMale
        imply adjGender GrammarNeuter
      when (w `endsWith` ["ой", "ей"]) $ imply adjGender GrammarFemale
    propose adjQuantity GrammarMultiple $ do
      when (w `endsWith` ["ых", "их"]) implyNothing

  propose adjCase Dativus $ do 
    propose adjQuantity GrammarSingle $ do
      when (w `endsWith` ["ому", "eму"]) $ do 
        imply adjGender GrammarMale
        imply adjGender GrammarNeuter
      when (w `endsWith` ["ой", "ей"]) $ imply adjGender GrammarFemale
    propose adjQuantity GrammarMultiple $ do
      when (w `endsWith` ["ым", "им"]) implyNothing

  propose adjCase Accusativus $ do 
    propose adjQuantity GrammarSingle $ do
      when (w `endsWith` ["ой", "ый", "ий"]) $ imply adjGender GrammarMale
      when (w `endsWith` ["оe", "ee"]) $ imply adjGender GrammarNeuter
      when (w `endsWith` ["ого", "eго"]) $ do 
        imply adjGender GrammarMale
        imply adjGender GrammarNeuter
      when (w `endsWith` ["ую", "юю"]) $ imply adjGender GrammarFemale
    propose adjQuantity GrammarMultiple $ do
      when (w `endsWith` ["ые", "ие", "ых", "их"]) implyNothing
        
  propose adjCase Ablativus $ do
    propose adjQuantity GrammarSingle $ do
      when (w `endsWith` ["ым", "им"]) $ do 
        imply adjGender GrammarMale
        imply adjGender GrammarNeuter
      when (w `endsWith` ["ой", "ою", "ей", "ею"]) $ imply adjGender GrammarFemale
    propose adjQuantity GrammarMultiple $ do
      when (w `endsWith` ["ыми", "ими"]) implyNothing

  propose adjCase Praepositionalis $ do
    propose adjQuantity GrammarSingle $ do
      when (w `endsWith` ["ом", "eм"]) $ do 
        imply adjGender GrammarMale
        imply adjGender GrammarNeuter
      when (w `endsWith` ["ой", "ей"]) $ imply adjGender GrammarFemale
    propose adjQuantity GrammarMultiple $ do
      when (w `endsWith` ["ых", "их"]) implyNothing

-- | Tries to guess verb properties by endings
verb :: Text -> Rule VerbProperties
verb w = do 
  infinitive 
  conjugation
  pastTime
  where

  -- Infinitive endings
  infinitive = do 
    propose verbMood ModusInfinitivus $ do 
      when (w `endsWith` ["еть", "овать", "ать", "ять"]) $ imply verbConjugation FirstConjugation -- with many exceptions

  -- Guess by conjugation 
  conjugation = proposeMany verbTime [PresentTime, FutureTime] $ do  
    propose verbPerson FirstPerson $ do 
      propose verbQuantity GrammarSingle $ do
        when (w `endsWith` ["у", "ю"]) $ do
          imply verbConjugation FirstConjugation
          imply verbConjugation SecondConjugation
      propose verbQuantity GrammarMultiple $ do 
        when (w `endsWith` ["ем"]) $ imply verbConjugation FirstConjugation
        when (w `endsWith` ["им"]) $ imply verbConjugation SecondConjugation
    propose verbPerson SecondPerson $ do 
      propose verbQuantity GrammarSingle $ do 
        when (w `endsWith` ["ешь"]) $ imply verbConjugation FirstConjugation
        when (w `endsWith` ["ишь"]) $ imply verbConjugation SecondConjugation
      propose verbQuantity GrammarMultiple $ do 
        when (w `endsWith` ["ете"]) $ imply verbConjugation FirstConjugation
        when (w `endsWith` ["ите"]) $ imply verbConjugation SecondConjugation
    propose verbPerson ThirdPerson $ do 
      propose verbQuantity GrammarSingle $ do 
        when (w `endsWith` ["ет"]) $ imply verbConjugation FirstConjugation
        when (w `endsWith` ["ит"]) $ imply verbConjugation SecondConjugation
      propose verbQuantity GrammarMultiple $ do 
        when (w `endsWith` ["ут", "ют"]) $ imply verbConjugation FirstConjugation
        when (w `endsWith` ["ат", "ят"]) $ imply verbConjugation SecondConjugation

  pastTime = propose verbTime PastTime $ do 
    when (w `endsWith` ["л"]) $ imply verbGender GrammarMale
    when (w `endsWith` ["ла"]) $ imply verbGender GrammarFemale
    when (w `endsWith` ["ли"]) $ imply verbQuantity GrammarMultiple

adverb :: Text -> Rule AdverbProperties
adverb w = do 
  when (w `endsWith` ["о"]) $ imply adverbDegree PositiveDegree
  when (w `endsWith` ["е"]) $ imply adverbDegree ComparitiveDegree