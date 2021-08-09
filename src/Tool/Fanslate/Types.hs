{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Tool.Fanslate.Types where

import Data.Aeson
import Servant.API
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (intercalate)

data Lang =
  SimplifiedChinese | English

data Google
data Baidu

instance ToHttpApiData Lang where
  toUrlPiece = T.pack . show

instance Show Lang where
  show SimplifiedChinese = "zh-CN"
  show English = "en"

instance FromJSON Lang where
  parseJSON (String "zh") = pure SimplifiedChinese
  parseJSON (String "en") = pure English
  parseJSON (String _) = fail "TBD"
  parseJSON _ = fail "Expected a string as language"

newtype SourceLang = SL Lang deriving (ToHttpApiData)
newtype TargetLang = TL Lang deriving (ToHttpApiData)
newtype Content = Content Text deriving (ToHttpApiData)

type family ResponseContent a
type instance ResponseContent Google = GRAlternativeTranslation

-- | Google Result: Alternative
newtype GRAlternativeItem = GRAI {
  wordPostProc :: Text
}

instance Show GRAlternativeItem where
  show = T.unpack . wordPostProc

instance FromJSON GRAlternativeItem where
  parseJSON = withObject "AlternativeItem" $ \o ->
    GRAI <$> o .: "word_postproc"

newtype GRAlternative = GRA {
  alternative :: [GRAlternativeItem]
}

instance Show GRAlternative where
  show = intercalate ", " . map show . alternative

instance FromJSON GRAlternative where
  parseJSON = withObject "Alternative" $ \o ->
    o .: "alternative" >>= fmap GRA . parseJSON

newtype GRAlternativeTranslation = GRAT {
  translations :: [GRAlternative]
}

instance Show GRAlternativeTranslation where
  show = show . map show . translations

instance FromJSON GRAlternativeTranslation where
  parseJSON = withObject "Alternative translation" $ \o ->
    o .: "alternative_translations" >>= fmap GRAT . parseJSON
