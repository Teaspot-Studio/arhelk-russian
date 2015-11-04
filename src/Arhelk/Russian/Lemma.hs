module Arhelk.Russian.Lemma(
    Lemma(..)
  , Word(..)
  , makeSentence
  , lemmanize
  , lemmaWord
  , module X
  ) where 

import Arhelk.Core.Rule
import Arhelk.Russian.Lemma.Adjective as X
import Arhelk.Russian.Lemma.Adverb as X
import Arhelk.Russian.Lemma.Data as X
import Arhelk.Russian.Lemma.Particle as X
import Arhelk.Russian.Lemma.Substantive as X
import Arhelk.Russian.Lemma.Verb as X
import Control.Monad
import Data.Maybe 
import Data.Text as T
import Lens.Simple
import Prelude hiding (Word)
import qualified Arhelk.Lexer as L
import qualified Data.List as DL
import TextShow

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

instance TextShow Word where 
  showb w = case w of 
    OneWord{..} -> showb wordSource 
    MultiWord{..} -> showb (T.unwords wordSources)

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

instance TextShow a => TextShow (Lemma a) where 
  showb p = case p of 
    UnknownWord a -> showb a <> " (?)"
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

-- | Sentence clause, region enclosed with commas
type SentenceClause a = [Lemma a]

-- | Any sentence is list of sentence clauses
type Sentence a = [SentenceClause a]

-- | Transforms list of tokens into sentence with clauses
makeSentence :: [L.Token ()] -> Sentence Text 
makeSentence ts = go [] ts 
  where
    go acc [] = acc 
    go acc tss = let 
      (clauses, tss') = getClause tss
      in case tss' of 
        [] -> acc ++ clauses
        _ -> go (acc ++ clauses) tss'

    getClause :: [L.Token ()] -> ([SentenceClause Text], [L.Token ()])
    getClause tss = case DL.findIndex isClauseBound tss of
      Nothing -> ([makeClause tss], [])
      Just i -> let
        (pre, x:post) = DL.splitAt i tss
        in case x of 
          L.Quotation qs -> (makeClause pre : makeSentence qs, post)
          _ -> ([makeClause pre], post)
      where
        isClauseBound a = a == L.Comma || a == L.Semicolon || a == L.Citation || a == L.Dash || isQuotation a 
        isQuotation (L.Quotation _) = True 
        isQuotation _ = False

    makeClause :: [L.Token ()] -> SentenceClause Text 
    makeClause = catMaybes . fmap processToken
      where
        processToken t = case t of 
          L.Word a -> Just $ UnknownWord a 
          _ -> Nothing

-- | Try to parse sentence
lemmanize :: Sentence Text -> Sentence Word
lemmanize = error "unimplemented"