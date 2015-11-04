module Arhelk.Russian.Lemma.Data(
    RawWord
  , SemiProcWord(..)
  , Word(..)
  , Lemma(..)
  , SentenceClause
  , Sentence 
  , lemmaWord
  , MoreSpecialized(..)
  , glueLessSpecialized
  , module X
  ) where

import Arhelk.Russian.Lemma.Data.Adjective as X 
import Arhelk.Russian.Lemma.Data.Adverb as X 
import Arhelk.Russian.Lemma.Data.Common as X
import Arhelk.Russian.Lemma.Data.Particle as X
import Arhelk.Russian.Lemma.Data.Substantive as X
import Arhelk.Russian.Lemma.Data.Verb as X 
import Data.Monoid 
import Data.Text as T
import Data.Type.Equality
import Prelude as P hiding (Word)
import qualified Data.Foldable as F 
import TextShow

-- | Raw word, unproccessed yet
type RawWord = Text 

-- | Shortcut for semiprocessed word
newtype SemiProcWord = SemiProcWord (Either RawWord Word)
  deriving (Eq, Show)

instance TextShow SemiProcWord where 
  showb (SemiProcWord (Left t)) = fromText t <> " (?)"
  showb (SemiProcWord (Right w)) = showb w 

-- | Parsed word with detached prefixes, postfixes and determinied speach part
data Word = 
    -- | Lemma from single word
    OneWord {
    -- | Initial full word
      wordSource :: Text
    -- | Semanitc root
    , wordRoot :: Text
    -- | Semantic prefixes
    , wordPrefixes :: [Text]
    -- | Semantic postfixes
    , wordPostfixes :: [Text]
    -- | Semantic ending
    , wordEndings :: Text
    }
  -- | Lemma from many words (ex. particles)
  | MultiWord {
    -- | Initial full word
      wordSources :: [Text]
    }
  deriving (Eq, Show)

-- | Total function to get original string of word
getWordSource :: Word -> Text 
getWordSource OneWord{..} = wordSource
getWordSource MultiWord{..} = T.unwords wordSources

instance TextShow Word where 
  showb w = case w of 
    OneWord{..} -> fromText wordSource 
    MultiWord{..} -> fromText (T.unwords wordSources)

-- | Lemma is a single semantic unit (list or words) that
-- correspond to speach part
data Lemma a = 
    UnknownWord a -- ^ Unknown part of speach 
  | Substantive a SubstantiveProperties -- ^ Существительное
  | Adjective a AdjectiveProperties -- ^ Прилагательное
  | Numeral a -- ^ Числительное
  | Pronoun a -- ^ Местоимение
  | Verb a VerbProperties -- ^ Глагол
  | Adverb a AdverbProperties -- ^ Наречие
  | Preposition a -- ^ Предлог
  | Conjunction a -- ^ Союз
  | GrammarParticle a ParticleProperties -- ^ Частица
  | Interjection a -- ^ Междуметие
  | Participle a  -- ^ Причастие
  | Transgressive a -- ^ Деепричастие
  deriving (Eq, Show)

-- | Sentence clause, region enclosed with commas
type SentenceClause a = [Lemma a]

-- | Any sentence is list of sentence clauses
type Sentence a = [SentenceClause a]

instance (TextShow a, (a == Text) ~ False)
  => TextShow (Lemma a) where 
  showb p = case p of 
    UnknownWord a -> showb a
    Substantive a p -> showb a <> " (сущ., " <> showb p <> ")"
    Adjective a p -> showb a <> " (прил.," <> showb p <> ")"
    Numeral a -> showb a <> " (числ.)"
    Pronoun a -> showb a <> " (мест.)"
    Verb a p -> showb a <> " (гл., " <> showb p <> ")"
    Adverb a p -> showb a <> " (нар., " <> showb p <> ")"
    Preposition a -> showb a <> " (предл.)"
    Conjunction a -> showb a <> " (союз)"
    GrammarParticle a p -> showb a <> " (част., " <> showb p <> ")"
    Interjection a -> showb a <> " (межд.)"
    Participle a -> showb a <> " (прич.)"
    Transgressive a -> showb a <> " (деепр.)"

instance TextShow (Lemma Text) where
  showb p = case p of 
    UnknownWord a -> fromText a
    Substantive a p -> fromText a <> " (сущ., " <> showb p <> ")"
    Adjective a p -> fromText a <> " (прил.," <> showb p <> ")"
    Numeral a -> fromText a <> " (числ.)"
    Pronoun a -> fromText a <> " (мест.)"
    Verb a p -> fromText a <> " (гл., " <> showb p <> ")"
    Adverb a p -> fromText a <> " (нар., " <> showb p <> ")"
    Preposition a -> fromText a <> " (предл.)"
    Conjunction a -> fromText a <> " (союз)"
    GrammarParticle a p -> fromText a <> " (част., " <> showb p <> ")"
    Interjection a -> fromText a <> " (межд.)"
    Participle a -> fromText a <> " (прич.)"
    Transgressive a -> fromText a <> " (деепр.)"

-- | Extract lemma content
lemmaWord :: Lemma a -> a 
lemmaWord l = case l of 
  UnknownWord a -> a
  Substantive a _ -> a
  Adjective a _ -> a
  Numeral a -> a
  Pronoun a -> a
  Verb a _ -> a
  Adverb a _ -> a
  Preposition a -> a
  Conjunction a -> a
  GrammarParticle a _ -> a
  Interjection a -> a
  Participle a -> a
  Transgressive a -> a

class Eq a => MoreSpecialized a where 
  -- | Check if first value is same as value but provides more info
  moreSpecialized :: a -> a -> Bool 

instance MoreSpecialized SemiProcWord where 
  moreSpecialized (SemiProcWord (Right a)) (SemiProcWord (Left b)) = getWordSource a == b 
  moreSpecialized a b = a == b 

instance MoreSpecialized a => MoreSpecialized (Lemma a) where
  moreSpecialized a (UnknownWord b) = moreSpecialized (lemmaWord a) b
  moreSpecialized a b = a == b 

instance MoreSpecialized a => MoreSpecialized [a] where 
  moreSpecialized a b 
    | P.length a /= P.length b = False
    | otherwise = and $ uncurry moreSpecialized <$> a `P.zip` b 

glueLessSpecialized :: MoreSpecialized a => [a] -> [a]
glueLessSpecialized = P.reverse . F.foldl' go [] 
  where
    go !acc a = if or $ fmap (`moreSpecialized` a) acc 
      then acc 
      else a : P.filter (not . (a `moreSpecialized`)) acc