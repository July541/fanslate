{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}
module Tool.Fanslate.Google.Type where

import           Data.Default
import           Data.Text                (Text)
import           Servant.Client
import           Tool.Fanslate.Google.Api ()
import           Tool.Fanslate.Types
import           Tool.Fanslate.Utils
import qualified Data.Text as T
import Data.Aeson
import Data.List
import Data.List.Extra
import Data.Maybe

data GoogleExtraParam = GoogleExtraParam {
  client :: Text
, dt1    :: Text
, dt2    :: Text
, dj     :: Text
}

type instance ExtraParams 'Google = GoogleExtraParam
type instance ClientReq 'Google = Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> BaseReq 'Google

instance Default (TranslateConfig 'Google) where
  def = TC
    (BaseUrl Http "translate.google.cn" 80 "")
    (Servant.Client.client $ proxy @'Google)
    (Just $ GoogleExtraParam "gtx" "at" "ex" "1")
    (\(Just (GoogleExtraParam a b c d)) f -> f (Just a) (Just b) (Just c) (Just d))

type instance ResponseContent 'Google = GoogleResult

-- | Google Result: Alternative item
newtype GRAlternativeItem = GRAI {
  wordPostProc :: Text
}

instance Show GRAlternativeItem where
  show = T.unpack . wordPostProc

instance FromJSON GRAlternativeItem where
  parseJSON = withObject "Google result alternative item" $ \o ->
    GRAI <$> o .: "word_postproc"

newtype GRAlternative = GRA {
  alternative :: [GRAlternativeItem]
}

instance Show GRAlternative where
  show = intercalate ", " . map show . alternative

instance FromJSON GRAlternative where
  parseJSON = withObject "Google result alternative" $ \o ->
    o .: "alternative" >>= fmap GRA . parseJSON

newtype GRExample = GRE {
  example :: Text
}

instance Show GRExample where
  show = T.unpack . example

instance FromJSON GRExample where
  parseJSON = withObject "Google result example" $ \o ->
    o .: "text"
      >>= fmap GRE . parseJSON

data GoogleResult = GoogleResult {
  translations :: [GRAlternative]
, examples :: Maybe [GRExample]
}

instance Show GoogleResult where
  show (GoogleResult a b)= let ex = fmap (map show) b
                           in show $ map show a <> fromMaybe [] ex

instance FromJSON GoogleResult where
  parseJSON = withObject "Alternative translation" $ \o -> do
    s <- o .: "alternative_translations" >>= parseJSON
    g <- o .:? "examples" >>= \case
      Just t -> t .: "example"  >>= parseJSON
      Nothing -> pure Nothing
    let r = case g of
          Just r -> Just $ map (\(GRE t) -> GRE (T.pack $ replace "</b>" "" $ replace "<b>" "" $ T.unpack t)) r
          Nothing -> Nothing
    return (GoogleResult s r)

