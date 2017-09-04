{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module Data.Machine.Source.Google.Storage where

import Data.ByteString.Lazy (ByteString)
import Data.Maybe (isJust, fromJust)
import Data.Monoid ((<>))
import Data.Machine.Plan (PlanT)
import Data.Text (Text)
import Network.Google (HasEnv)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans (MonadTrans(lift))
import Control.Monad.Trans.Resource
       (MonadBaseControl, MonadResource, ResourceT, liftResourceT,
        runResourceT)
import Control.Lens ((&), (^.), (?~))
import Control.Monad.Catch (MonadCatch(catch), MonadThrow)
import Network.Google.Auth.Scope (AllowScopes, HasScope')
import Network.HTTP.Types (urlEncode)

import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Network.Google as Google
import qualified Network.Google.Storage as Storage
import qualified Data.Machine as Machine
import qualified Data.Conduit as Conduit
import qualified Data.Conduit.Binary as Conduit


-- | Yield data from Google Cloud Storage's bucket with 'bucketName'
-- where object names is match with 'objectName'.
--
-- You can use wildcard '*' for subsctitude any word in name.
-- Name can't contains '/' symbol.
fromGCS ::
     ( HasScope' s '[ "https://www.googleapis.com/auth/cloud-platform"
                    , "https://www.googleapis.com/auth/cloud-platform.read-only"
                    , "https://www.googleapis.com/auth/devstorage.full_control"
                    , "https://www.googleapis.com/auth/devstorage.read_only"
                    , "https://www.googleapis.com/auth/devstorage.read_write"] ~ 'True
     , AllowScopes s
     , HasEnv s r
     , MonadResource m
     , MonadIO m
     , m ~ ResourceT IO
     )
  => r
  -> Text
  -> Text
  -> PlanT k ByteString m b
fromGCS env bucketName objectName =
  lift (parseObjectName env bucketName objectName) >>= go
  where
    go [] = Machine.stop
    go (x:xs) =
      lift
        (Google.runGoogle
           env
           (Google.download
              (Storage.objectsGet
                 bucketName
                 (Text.decodeUtf8 (urlEncode False (Text.encodeUtf8 x))))) >>= \stream ->
           stream Conduit.$$+- Conduit.sinkLbs) >>=
      Machine.yield >>
      go xs

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
  in if objName == prefix
       then do
         objsName <-
           runResourceT $
           Google.runGoogle
             env
             (Google.catching
                Google._Error
                (Google.send
                   (Storage.objectsList bucketName & Storage.olPrefix ?~ objName))
                (\_ -> return Storage.objects))
         return
           (map
              (\oitem -> fromJust (oitem ^. Storage.objName))
              (filter
                 (\oitem -> isJust (oitem ^. Storage.objName))
                 (objsName ^. Storage.oItems)))
       else do
         objsName <-
           runResourceT $
           Google.runGoogle
             env
             (Google.catching
                Google._Error
                (Google.send
                   (Storage.objectsList bucketName & Storage.olPrefix ?~ prefix &
                    Storage.olDelimiter ?~
                    "/"))
                (\_ -> return Storage.objects))
         mapM
           (\oprefix ->
              parseObjectName env bucketName (resolve prefix oprefix objName))
           (objsName ^. Storage.oPrefixes) >>=
           return . concat


-- | Substitute prefix plus `*/` with oprefix in objName
--
-- >>> resolve "prefix/" "prefix/o/" "prefix/*/string"
-- "prefix/o/string"
resolve :: Text -> Text -> Text -> Text
resolve prefix oprefix objName =
  oprefix <> Text.drop (2 + Text.length prefix) objName

-- | Prefix is everything before first `*` symbol.
--
-- >>> parsePrefix "prefix/*/suffix"
-- "prefix/"
--
-- >>> parsePrefix "prefix/*/suffix/*/suffix"
-- "prefix/"
parsePrefix :: Text -> Text
parsePrefix = Text.takeWhile (/= '*')
