{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
module Tool.Fanslate.Types where

import           Data.Aeson
import           Data.List      (intercalate)
import           Data.Text      (Text)
import qualified Data.Text      as T
import           Servant.API
import           Servant.Client

data Lang =
  SimplifiedChinese | English

data Site = Baidu | Google

type family API (a :: Site)
type family ExtraParams (a :: Site)

-- | Base translate request
type BaseReq (a :: Site) = Maybe SourceLang -> Maybe TargetLang -> Maybe Content -> ClientM (ResponseContent a)

type family ClientReq (a :: Site)

data TranslateConfig (a :: Site) = TC {
  url               :: BaseUrl
, api               :: ClientReq a
, extraParams       :: Maybe (ExtraParams a)
, renderExtraParams :: Maybe (ExtraParams a) -> ClientReq a -> BaseReq a
}

instance ToHttpApiData Lang where
  toUrlPiece = T.pack . show

instance Show Lang where
  show SimplifiedChinese = "zh-CN"
  show English           = "en"

instance FromJSON Lang where
  parseJSON (String "zh") = pure SimplifiedChinese
  parseJSON (String "en") = pure English
  parseJSON (String _)    = fail "TBD"
  parseJSON _             = fail "Expected a string as language"

newtype SourceLang = SL Lang deriving (ToHttpApiData)
newtype TargetLang = TL Lang deriving (ToHttpApiData)
newtype Content = Content Text deriving (ToHttpApiData)
type family ResponseContent (a :: Site)
