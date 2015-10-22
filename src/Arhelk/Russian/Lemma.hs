module Arhelk.Russian.Lemma(
    SpeachPart(..)
  , Lemma(..)
  , lemmanize
  ) where 

import Data.Text 

-- | Describes possible part of speach in Russian
data SpeachPart = 
    Substantive
  | Adjective
  | Numeral
  | Pronoun
  | Verb
  | Adverb
  | Preposition
  | Conjunction
  | GrammarParticle
  | Interjection
  | Participle
  | Transgressive
  deriving (Eq, Ord, Enum, Show)

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