{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module Data.Machine.Source.Google.Storage where

import Data.Monoid ((<>))
import Data.Text (Text)
import Network.Google (HasEnv)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Resource (runResourceT, MonadBaseControl)
import Control.Lens ((&), (.~), (^.))
import Control.Monad.Catch (MonadThrow, MonadCatch(catch))
import Network.Google.Auth.Scope (AllowScopes, HasScope')

import qualified Data.Text as Text
import qualified Network.Google as Google
import qualified Network.Google.Storage as Storage


-- | So in 'fromGCS' you can use wildcard `*` for objectName
-- but 'Network.Google.Storage' doesn't support wildcards hence
-- we have to resolve them.
--
-- We know that wildcard `*` can code only one word. First we find
-- all symbols before `*`, we called it prefix, when prefix equal to objName
-- that means there is no wildcards and we doesn't have to resolve something and
-- we return `objName` but we get list of objects in GCS with given bucket and prefix
-- and continue to resolve object names where substitute founded wildcard with every
-- object name founded in GCS.
--
-- Example: we know that in GCS we have folders:
--  some-bucket-name/objectparent/objectson/objectgrandson
--  some-bucket-name/objectparent/objectson/objectgranddaughter
--  some-bucket-name/objectparent/objectdaughter/objectgrandson
--  some-bucket-name/objectparent/objectdaughter/objectgrandson
--
-- and parseObjectName some-bucket-name objectparent/*/objectgranddaughter
-- returns [objectparent/objectson/objectgranddaughter, objectparent/objectdaughter/objectgranddaughter]
parseObjectName ::
     ( HasScope' s '[ "https://www.googleapis.com/auth/cloud-platform"
                    , "https://www.googleapis.com/auth/cloud-platform.read-only"
                    , "https://www.googleapis.com/auth/devstorage.full_control"
                    , "https://www.googleapis.com/auth/devstorage.read_only"
                    , "https://www.googleapis.com/auth/devstorage.read_write"] ~ 'True
     , AllowScopes s
     , HasEnv s r
     , MonadIO m
     , MonadThrow m
     , MonadCatch m
     , MonadBaseControl IO m
     )
  => r
  -> Text
  -> Text
  -> m [Text]
parseObjectName env bucketName objName =
  let prefix = parsePrefix objName
  in if objName == parsePrefix objName
       then return [objName]
       else do
         objsName <-
           runResourceT $
           Google.runGoogle
             env
             (Google.catching
                Google._Error
                (Google.send
                   (Storage.objectsList bucketName & Storage.olPrefix .~
                    Just prefix &
                    Storage.olDelimiter .~
                    Just "/"))
                (\_ -> return Storage.objects))
         mapM
           (\oprefix -> parseObjectName env bucketName (resolve prefix oprefix objName ))
           (objsName ^. Storage.oPrefixes) >>=
           return . concat


-- | Substitute prefix plus `*/` with oprefix in objName
--
-- >>> resolve "prefix/" "prefix/o/" "prefix/*/string"
-- "prefix/o/string"
resolve :: Text -> Text -> Text -> Text
resolve prefix oprefix objName = oprefix <> Text.drop (2 + Text.length prefix) objName

-- | Prefix is everything before first `*` symbol.
--
-- >>> parsePrefix "prefix/*/suffix"
-- "prefix/"
parsePrefix :: Text -> Text
parsePrefix = Text.takeWhile (/= '*')
