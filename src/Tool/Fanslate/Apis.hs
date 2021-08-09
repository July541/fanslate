{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Tool.Fanslate.Apis where

import Servant.API
import Data.Text
import Tool.Fanslate.Types

type family API a

type instance API Google = GoogleTranslateAPI

type GoogleTranslateAPI = "translate_a"
  :> "single"
  :> QueryParam "client" Text
  :> QueryParam "dt" Text
  :> QueryParam "dt" Text
  :> QueryParam "dj" Text
  :> QueryParam "sl" SourceLang
  :> QueryParam "tl" TargetLang
  :> QueryParam "q" Content
  :> Get '[JSON] (ResponseContent Google)
