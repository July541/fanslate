{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Tool.Fanslate.Source where

import Servant.Client
import Tool.Fanslate.Types
import Data.Proxy
import Network.HTTP.Client
import Data.Default
import Data.Text (Text)
import Tool.Fanslate.Apis

data TranslateSource a = TS {
  url :: BaseUrl
, api :: ApiClient a
, extraParams :: Maybe (ExtraParams a)
, renderExtraParams :: Maybe (ExtraParams a) -> ApiClient a -> BaseApi a
}

type family ExtraParams a
type instance ExtraParams Google = GoogleExtraParam

data GoogleExtraParam = GoogleExtraParam {
  client :: Text
, dt1 :: Text
, dt2 :: Text
, dj :: Text
}

type BaseApi a = Maybe SourceLang -> Maybe TargetLang -> Maybe Content -> ClientM (ResponseContent a)

type family ApiClient a
type instance ApiClient Google = Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> BaseApi Google

proxyApi :: Data.Proxy.Proxy (API a)
proxyApi = Data.Proxy.Proxy

translate :: TranslateSource a -> Manager -> SourceLang -> TargetLang -> Content -> IO (Either ClientError (ResponseContent a))
translate TS{..} m src target content =
  runClientM (renderExtraParams extraParams api (Just src) (Just target) (Just content)) (mkClientEnv m url)

instance Default (TranslateSource Google) where
  def = TS
    (BaseUrl Http "translate.google.cn" 80 "")
    (Servant.Client.client $ proxyApi @Google)
    (Just $ GoogleExtraParam "gtx" "at" "ex" "1")
    (\(Just (GoogleExtraParam a b c d)) f -> let t = f (Just a) (Just b) (Just c) (Just d) in t)

-- http://translate.google.com/translate_a/single?client=gtx&dt=at&dt=ex&ie=UTF-8&dj=1&sl=en&tl=zh-CN&q=book

run :: IO ()
run = do
  let st = defaultManagerSettings
  m <- newManager st
  g <- translate (def :: TranslateSource Google) m (SL English) (TL SimplifiedChinese) (Content "how old are you")
  case g of
    Right (GRAT x) -> print x
    Left _ ->  print g


