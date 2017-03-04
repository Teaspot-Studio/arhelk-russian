module Arhelk.Russian.Lemma.Adjective(
    adjective
  ) where

import Arhelk.Core.Rule
import Arhelk.Russian.Lemma.Common
import Arhelk.Russian.Lemma.Data
import Control.Monad
import Data.Text as T 

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