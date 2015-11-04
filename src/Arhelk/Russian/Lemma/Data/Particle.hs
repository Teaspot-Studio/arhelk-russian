module Arhelk.Russian.Lemma.Data.Particle where

import Data.Monoid
import TextShow 

data ParticleProperties = ParticleProperties
  deriving (Show, Eq)

instance Monoid ParticleProperties where
  mempty = ParticleProperties
  mappend a _ = a

instance TextShow ParticleProperties where
  showb _ = ""