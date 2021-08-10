{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Tool.Fanslate.Google.Api where

import           Data.Text           (Text)
import           Servant.API
import           Tool.Fanslate.Types

type instance API 'Google = GoogleTranslateAPI

type GoogleTranslateAPI = "translate_a"
  :> "single"
  :> QueryParam "client" Text
  :> QueryParam "dt" Text
  :> QueryParam "dt" Text
  :> QueryParam "dj" Text
  :> QueryParam "sl" SourceLang
  :> QueryParam "tl" TargetLang
  :> QueryParam "q" Content
  :> Get '[JSON] (ResponseContent 'Google)
