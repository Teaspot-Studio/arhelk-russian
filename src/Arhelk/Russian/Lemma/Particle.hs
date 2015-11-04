module Arhelk.Russian.Lemma.Particle(
    particle
  ) where

import Arhelk.Core.Rule
import Arhelk.Russian.Lemma.Common
import Arhelk.Russian.Lemma.Data
import Control.Arrow (first)
import Control.Monad
import Data.List (nub, nubBy)
import Data.Maybe 
import Data.Text as T 
import Prelude as P hiding (Word)
import qualified Data.Foldable as F 
import qualified Data.Text.IO as T 
import System.IO.Unsafe (unsafePerformIO)

-- | Particle contains many words
type Particle = [Text]

-- | Build in list of particles
particles :: [Particle]
particles = unsafePerformIO $ do 
  ts <- T.readFile "config/particles.txt"
  return $ T.words . T.strip <$> T.lines ts

{-# NOINLINE particles #-}

-- | Tries to find particles including multiword
particle :: SentenceClause SemiProcWord -> [SentenceClause SemiProcWord]
particle clause = glueLessSpecialized $ do
  clause' <- makeSingleHypothesis clause
  clause' : particle clause'
  where
    makeSingleHypothesis cl = catMaybes $ findParticle cl <$> particles

-- | Tries to find particle within unknown words in sentence
findParticle :: SentenceClause SemiProcWord -> Particle -> Maybe (SentenceClause SemiProcWord)
findParticle clause particle = let
  (preClause', mpw, postClause', leftParticle) = F.foldl' go ([], Nothing, [], particle) clause
  in case leftParticle of 
    [] -> case mpw of 
      Nothing -> Nothing
      Just pw -> Just $ P.reverse preClause' ++ [GrammarParticle (SemiProcWord $ Right pw) ParticleProperties] ++ P.reverse postClause'
    _ -> Nothing
  where
    go :: (SentenceClause SemiProcWord, Maybe Word, SentenceClause SemiProcWord, Particle) 
      -> Lemma SemiProcWord 
      -> (SentenceClause SemiProcWord, Maybe Word, SentenceClause SemiProcWord, Particle) 
    go (pre, pw, post, []) l = (pre, pw, l : post, [])
    go (pre, Nothing, post, ps) l = case l of -- first particle part isn't met yet
      UnknownWord (SemiProcWord (Left w)) -> if w `elem` ps 
        then (pre, Just (MultiWord [w]), post, P.filter (/= w) ps) 
        else (l : pre, Nothing, post , ps)
      _ -> (l : pre, Nothing, post , ps)
    go (pre, mpw@(Just (MultiWord pw)), post, ps) l = case l of -- already met particle parts
      UnknownWord (SemiProcWord (Left w)) -> if w `elem` ps 
        then (pre, Just (MultiWord $ pw ++ [w]), post, P.filter (/= w) ps) 
        else (post, mpw, l : post, ps)
      _ -> (post, mpw, l : post, ps)