{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RecordWildCards     #-}
module Tool.Fanslate.Utils where
import           Data.Proxy
import           Network.HTTP.Client (Manager)
import           Servant.Client
import           Tool.Fanslate.Types

proxy :: Proxy (API a)
proxy = Proxy

translate :: TranslateConfig a -> Manager -> SourceLang -> TargetLang
               -> Content -> IO (Either ClientError (ResponseContent a))
translate TC{..} m src target content =
  runClientM (renderExtraParams extraParams api (Just src) (Just target) (Just content)) (mkClientEnv m url)

