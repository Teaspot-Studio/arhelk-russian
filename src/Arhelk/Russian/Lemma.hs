module Arhelk.Russian.Lemma(
    makeSentence
  , lemmanize
  , lemmanizeClause
  , linerizeHypothesis
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
import Prelude as P hiding (Word)
import qualified Arhelk.Lexer as L
import qualified Data.List as DL
import TextShow

-- | Transforms list of tokens into sentence with clauses
makeSentence :: [L.Token ()] -> Sentence Text 
makeSentence ts = let s = go [] ts in DL.length s `seq` s
  where
    go !acc [] = acc 
    go !acc tss = let 
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
lemmanize :: Sentence Text -> [Sentence SemiProcWord]
lemmanize = linerizeHypothesis . fmap lemmanizeClause

-- | Generates hypothesis about sentence clause
lemmanizeClause :: SentenceClause Text -> [SentenceClause SemiProcWord]
lemmanizeClause st = substantive' ssw
  >>= adjective' 
  >>= verb'
  >>= adverb'
  >>= particle'
  where
    unproccessed = SemiProcWord . Left
    ssw = fmap unproccessed <$> st 

    substantive' = liftWordRule $ \w -> Substantive (unproccessed w) <$> runRule (substantive w)
    adjective' = liftWordRule $ \w -> Adjective (unproccessed w) <$> runRule (adjective w)
    verb' = liftWordRule $ \w -> Verb (unproccessed w) <$> runRule (verb w)
    adverb' = liftWordRule $ \w -> Adverb (unproccessed w) <$> runRule (adverb w) 
    particle' s = s : particle s 
    
-- | Transforms rule on single word to rule about clauses
liftWordRule :: (Text -> [Lemma SemiProcWord]) -> SentenceClause SemiProcWord -> [SentenceClause SemiProcWord]
liftWordRule wordRule = linerizeHypothesis . fmap genHyps
  where 
  genHyps l = case l of 
    UnknownWord (SemiProcWord (Left w)) -> l : wordRule w
    _ -> [l]

-- | This operations useful when you try to apply many rules to list of elements 
-- and want get total list of hypothesis.
--
-- One can consider the operation as Decart multiplication
-- 
-- Example:
-- >>> linerizeHypothesis [[1], [2, 3], [4]]
-- [[1, 2, 4], [1, 3, 4]]
--
-- >>> linerizeHypothesis [[1, 2], [3, 4]]
-- [[1, 3], [1, 4], [2, 3], [2, 4]]
linerizeHypothesis :: [[a]] -> [[a]]
linerizeHypothesis [] = []
linerizeHypothesis (x:xs) = fmap P.reverse $ DL.foldl' go ((\a->[a]) <$> x) xs
  where 
    go !accs as = do 
      acc <- accs 
      a <- as 
      return $ a : acc