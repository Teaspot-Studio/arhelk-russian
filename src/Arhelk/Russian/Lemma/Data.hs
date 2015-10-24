module Arhelk.Russian.Lemma.Data where

import Lens.Simple
import Data.Monoid
import TextShow 

-- | Describes possible part of speach in Russian
data SpeachPart = 
    Substantive -- ^ Существительное
  | Adjective -- ^ Прилагательное
  | Numeral -- ^ Числительное
  | Pronoun -- ^ Местоимение
  | Verb -- ^ Глагол
  | Adverb -- ^ Наречие
  | Preposition -- ^ Предлог
  | Conjunction -- ^ Союз
  | GrammarParticle -- ^ Частица
  | Interjection -- ^ Междуметие
  | Participle -- ^ Причастие
  | Transgressive -- ^ Деепричастие
  deriving (Eq, Ord, Enum, Show, Bounded)

instance TextShow SpeachPart where 
  showb p = case p of 
    Substantive -> "сущ."
    Adjective -> "прил."
    Numeral -> "числ."
    Pronoun -> "мест."
    Verb -> "гл."
    Adverb -> "нар."
    Preposition -> "предл."
    Conjunction -> "союз"
    GrammarParticle -> "част."
    Interjection -> "межд."
    Participle -> "прич."
    Transgressive -> "деепр."

-- | Склонение. Describes declension of substantives
data Declension = 
    FirstDeclension
  | SecondDeclension
  | ThirdDeclension
  deriving (Eq, Ord, Enum, Show, Bounded)

instance TextShow Declension where 
  showb v = case v of 
    FirstDeclension -> "I скл."
    SecondDeclension -> "II скл."
    ThirdDeclension -> "III скл."

-- | Падеж. Grammatical case.
data GrammarCase =
    Nominativus -- ^ Иминительный
  | Genitivus -- ^ Родительный
  | Dativus -- ^ Дательный
  | Accusativus -- ^ Винительный
  | Ablativus -- ^ Творительный
  | Praepositionalis -- ^ Предложный
  deriving (Eq, Ord, Enum, Show, Bounded)

instance TextShow GrammarCase where 
  showb v = case v of 
    Nominativus -> "им. падеж"
    Genitivus -> "род. падеж"
    Dativus -> "дат. падеж"
    Accusativus -> "вин. падеж"
    Ablativus -> "твор. падеж"
    Praepositionalis -> "предл. падеж"

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

-- | Имя нарицательное или собственное
data Appellativity =
    AppellativeNoun -- ^ Нарицательное
  | ProperNoun -- ^ Собственное
  deriving (Eq, Ord, Enum, Show, Bounded)

instance TextShow Appellativity where 
  showb v = case v of 
    AppellativeNoun -> "нариц."
    ProperNoun -> "собств."

-- | Одушевленность 
data Animacy = 
    AnimateNoun -- ^ Одушевленное
  | InanimateNoun -- ^ Неодушевленное
  deriving (Eq, Ord, Enum, Show, Bounded)

instance TextShow Animacy where 
  showb v = case v of
    AnimateNoun -> "одушвл."
    InanimateNoun -> "неодушвл."

-- | Substantive morphological properties
data SubstantiveProperties = SubstantiveProperties {
  _substAppellativity :: Maybe Appellativity
, _substAnimacy :: Maybe Animacy
, _substDeclension :: Maybe Declension
, _substGender :: Maybe GrammarGender
, _substQuantity :: Maybe GrammarQuantity
, _substCase :: Maybe GrammarCase
} deriving (Eq, Show)

$(makeLenses ''SubstantiveProperties)

instance Monoid SubstantiveProperties where 
  mempty = SubstantiveProperties {
    _substAppellativity = Nothing
  , _substAnimacy = Nothing
  , _substDeclension = Nothing
  , _substGender = Nothing
  , _substQuantity = Nothing
  , _substCase = Nothing
  }

  mappend a b = SubstantiveProperties {
    _substAppellativity = getFirst $ First (_substAppellativity a) <> First (_substAppellativity b)
  , _substAnimacy = getFirst $ First (_substAnimacy a) <> First (_substAnimacy b) 
  , _substDeclension = getFirst $ First (_substDeclension a) <> First (_substDeclension b)
  , _substGender = getFirst $ First (_substGender a) <> First (_substGender b)
  , _substQuantity = getFirst $ First (_substQuantity a) <> First (_substQuantity b)
  , _substCase = getFirst $ First (_substCase a) <> First (_substCase b)
  }

instance TextShow SubstantiveProperties where 
  showb SubstantiveProperties{..} = unwordsB [
      maybe "" showb _substAppellativity
    , maybe "" showb _substAnimacy
    , maybe "" showb _substDeclension
    , maybe "" showb _substGender
    , maybe "" showb _substQuantity
    , maybe "" showb _substCase
    ]