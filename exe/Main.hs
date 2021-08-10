{-# LANGUAGE DataKinds #-}
module Main where

import Fanslate
import Network.HTTP.Client
import Data.Default


runGoogle :: IO ()
runGoogle = do
  m <- newManager defaultManagerSettings
  r <- translate (def :: TranslateConfig 'Google) m (SL SimplifiedChinese) (TL English) (Content "好")
  case r of
    Right (GoogleResult a b) -> print a >> print b
    Left _         ->  print r

main :: IO ()
main = do
  runGoogle
