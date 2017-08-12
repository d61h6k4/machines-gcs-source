{-# LANGUAGE OverloadedStrings #-}
import System.IO (stdout)
import Control.Lens ((<&>), (.~))
import Control.Monad.Trans.Resource (runResourceT)

import qualified Data.Machine as Machine
import qualified Network.Google as Google
import qualified Network.Google.Storage as Storage

import Data.Machine.Source.Google.Storage

main :: IO ()
main = do
  lgr <- Google.newLogger Google.Debug stdout
  env <-
    Google.newEnv <&> (Google.envLogger .~ lgr) .
    (Google.envScopes .~ Storage.storageReadWriteScope)
  parseObjectName env "erregistro" "company/*/project/*/service/*/2017/08/07/" >>=
    print
  runResourceT
    (Machine.runT
       ((Machine.construct
           (fromGCS
              env
              "erregistro"
              "company/*/project/*/service/*/2017/08/07/")))) >>= print
  return ()
