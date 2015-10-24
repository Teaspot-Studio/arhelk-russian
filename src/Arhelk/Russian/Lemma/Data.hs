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

-- | Разряд прилагательного
data AdjectiveCategory = 
    QualitiveAdjective -- ^ Качественное
  | ComparativeAdjective -- ^ Относительное
  | PossessiveAdjective -- ^ Притяжательное
  deriving (Eq, Ord, Enum, Show, Bounded)

instance TextShow AdjectiveCategory where 
  showb v = case v of 
    QualitiveAdjective -> "качеств."
    ComparativeAdjective -> "сравн."
    PossessiveAdjective -> "притяж."

-- | Степень сравнения
data AdjectiveDegree =
    PositiveDegree -- ^ Положительная степерь
  | ComparitiveDegree -- ^ Сравнительная степень
  | SuperlativeDegree -- ^ Превосходная степень
  deriving (Eq, Ord, Enum, Show, Bounded)

instance TextShow AdjectiveDegree where 
  showb v = case v of 
    PositiveDegree -> "полож. степень"
    ComparitiveDegree -> "сравн. степень"
    SuperlativeDegree -> "превосх. степень"

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

data AdjectiveProperties = AdjectiveProperties {
  _adjGender :: Maybe GrammarGender
, _adjCase :: Maybe GrammarCase
, _adjQuantity :: Maybe GrammarQuantity
, _adjShort :: Maybe Bool
, _adjCategory :: Maybe AdjectiveCategory
}

$(makeLenses ''AdjectiveProperties)

instance Monoid AdjectiveProperties where 
  mempty = AdjectiveProperties {
    _adjGender = Nothing
  , _adjCase = Nothing
  , _adjQuantity = Nothing
  , _adjShort = Nothing
  , _adjCategory = Nothing
  }

  mappend a b = AdjectiveProperties {
    _adjGender = getFirst $ First (_adjGender a) <> First (_adjGender b)
  , _adjCase = getFirst $ First (_adjCase a) <> First (_adjCase b)
  , _adjQuantity = getFirst $ First (_adjQuantity a) <> First (_adjQuantity b)
  , _adjShort = getFirst $ First (_adjShort a) <> First (_adjShort b)
  , _adjCategory = getFirst $ First (_adjCategory a) <> First (_adjCategory b)
  }

instance TextShow AdjectiveProperties where
  showb AdjectiveProperties{..} = unwordsB [
      maybe "" showb _adjGender
    , maybe "" showb _adjCase
    , maybe "" showb _adjQuantity
    , maybe "" showb _adjShort
    , maybe "" showb _adjCategory
    ]