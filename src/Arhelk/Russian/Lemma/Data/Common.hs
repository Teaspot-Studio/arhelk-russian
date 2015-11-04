module Arhelk.Russian.Lemma.Data.Common where

import TextShow 

-- | Род. Grammatical gender of word
data GrammarGender = 
    GrammarMale -- ^ Мужской
  | GrammarFemale -- ^ Женский
  | GrammarNeuter -- ^ Средний
  deriving (Eq, Ord, Enum, Show, Bounded)

instance TextShow GrammarGender where 
  showb v = case v of 
    GrammarMale -> "муж. род"
    GrammarFemale -> "жен. род"
    GrammarNeuter -> "ср. род"

-- | Множественное или единственное число
data GrammarQuantity =
    GrammarSingle -- ^ Единственное
  | GrammarMultiple -- ^ Множественное
  deriving (Eq, Ord, Enum, Show, Bounded)

instance TextShow GrammarQuantity where 
  showb v = case v of 
    GrammarSingle -> "ед. число"
    GrammarMultiple -> "мн. число"

-- | Лицо
data GrammarPerson =
    FirstPerson -- ^ Первое лицо
  | SecondPerson -- ^ Второе лицо
  | ThirdPerson -- ^ Третье лицо
  deriving (Eq, Ord, Enum, Show, Bounded)

instance TextShow GrammarPerson where 
  showb v = case v of 
    FirstPerson -> "1 лицо"
    SecondPerson -> "2 лицо"
    ThirdPerson -> "3 лицо"
