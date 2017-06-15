{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Main where

import           Conduit
import           Control.Arrow
import           Control.Exception                          hiding (Handler)
import           Control.Monad
import           Control.Monad.Trans.Resource
import           Data.Aeson                                 as Aeson
import           Data.Aeson.Encoding                        as Aeson
import           Data.ByteString                            (ByteString)
import qualified Data.ByteString                            as ByteString
import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy                       as ByteString.Lazy
import           Data.Conduit
import           Data.Conduit.Binary
import qualified Data.Conduit.Combinators                   as C
import qualified Data.Conduit.List                          as CL
import           Data.Maybe
import           Data.Monoid
import qualified Data.Vector                                as Vector
import           GHC.Word
import           Network.HTTP.Media                         (MediaType)
import qualified Network.HTTP.Media                         as Media
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Conduit
import           Network.Wai.Handler.Warp
import           Say
import           Servant
import           Servant.API.ContentTypes
import           Servant.Server
import           Servant.Server.Internal.Router
import           Servant.Server.Internal.RoutingApplication

data GetSource a
data PostConduit i o

class Show a => Streamable a where
  streamableCT :: Proxy a -> Maybe MediaType
  streamableCT _ = Nothing
  streamableOutDelimiter :: Proxy a -> Maybe Builder
  streamableOutDelimiter _ = Nothing
  streamableToBuilder :: a -> Builder
  streamableFromByteString :: ByteString -> Maybe a

class StreamableOut a where

toBuilderDelimited :: forall a. Streamable a => a -> [Flush Builder]
toBuilderDelimited a = Chunk (streamableToBuilder a)
                       : case streamableOutDelimiter (Proxy :: Proxy a) of
                             Just delim -> [Chunk delim, Flush]
                             Nothing    -> [Flush]

instance Streamable Aeson.Value where
  streamableToBuilder = lazyByteString . encodingToLazyByteString . toEncoding
  streamableCT _ = Just ("application" Media.// "x-json-stream")
  streamableOutDelimiter _ = Just (lazyByteString "\n")
  streamableFromByteString = decode . ByteString.Lazy.fromStrict

instance Streamable ByteString where
  streamableToBuilder = lazyByteString . ByteString.Lazy.fromStrict
  streamableFromByteString = Just

instance Streamable a => HasServer (GetSource a) ctxt where
  type ServerT (GetSource a) m = m (Source (ResourceT IO) a)

  route Proxy _ctxt sub = leafRouter $
    \env req k ->
      bracket createInternalState
              closeInternalState
              (runAction sub env req k . mkResponse)
    where

      mkResponse :: InternalState
                 -> Source (ResourceT IO) a
                 -> RouteResult Response
      mkResponse st =
        Route
        . responseSource ok200 headers
        . (.| CL.map toBuilderDelimited .| C.concat)
        . transPipe (`runInternalState` st)

      headers :: ResponseHeaders
      headers =
        let maybeMediaTypeBS = Media.renderHeader <$> streamableCT (Proxy :: Proxy a)
        in maybeToList $ ("Content-Type",) <$> maybeMediaTypeBS

instance (Streamable i, Streamable o) => HasServer (PostConduit i o) ctxt where
  type ServerT (PostConduit i o) m = m (ConduitM i o (ResourceT IO) ())

  route Proxy _ctxt sub = leafRouter $
    \env req k ->
      bracket createInternalState
              closeInternalState
              (runAction sub env req k . transform req)
    where

      transform :: Request
                -> InternalState
                -> ConduitM i o (ResourceT IO) ()
                -> RouteResult Response
      transform req st transformer =
        let src = sourceRequestBody req
                  .| Data.Conduit.Binary.lines
                  .| mapC streamableFromByteString
                  .| CL.catMaybes
                  .| transformer
        in mkResponse st src

      mkResponse :: InternalState
                 -> ConduitM () o (ResourceT IO) ()
                 -> RouteResult Response
      mkResponse st =
        Route
        . responseSource ok200 headers
        . (.| CL.map toBuilderDelimited .| C.concat)
        . transPipe (`runInternalState` st)

      headers :: ResponseHeaders
      headers =
        let maybeMediaTypeBS = Media.renderHeader <$> streamableCT (Proxy :: Proxy o)
        in maybeToList $ ("Content-Type",) <$> maybeMediaTypeBS

-- | This type defines the REST API for the server.
type ServiceAPI = "one" :> Get '[JSON] Int
   :<|> "ones" :> GetSource ByteString
   :<|> "hello" :> GetSource Aeson.Value
   :<|> "identity" :> PostConduit Aeson.Value Aeson.Value
   :<|> "inc" :> PostConduit Aeson.Value Aeson.Value

-- | Define the Warp server.
serveAPI :: Server ServiceAPI
serveAPI =
  serveOne
  :<|> serveOnes
  :<|> serveHello
  :<|> serveIdentity
  :<|> serveInc

serveOne :: Handler Int
serveOne = return 1

serveOnes :: Handler (Source (ResourceT IO) ByteString)
serveOnes =
  return go
  where go = do
          yield "1"
          go

serveHello :: Handler (Source (ResourceT IO) Aeson.Value)
serveHello = return go
  where go = do
          yield (Array (Vector.replicate 10 (String "hello")))
          go

serveIdentity :: Handler (ConduitM Aeson.Value Aeson.Value (ResourceT IO) ())
serveIdentity = return go
  where go = await >>= \case
          Just x  -> yield x >> go
          Nothing -> return ()

serveInc :: Handler (ConduitM Aeson.Value Aeson.Value (ResourceT IO) ())
serveInc = return go
  where go = await >>= \case
          Just (Aeson.Number x)  -> yield (Aeson.String $ x+1 ) >> go
          Just (Aeson.String x)  -> yield (Aeson.String $ x<>"!") >> go
          Just x  -> error $ "serveInc: unsupported constructor " <> show x
          Nothing -> return ()

main :: IO ()
main = do
  let port = 8080
  putStrLn $ "Going to listen port " <> show port
  run port (serve (Proxy :: Proxy ServiceAPI) serveAPI)
  return ()


-- splitOn :: Monad m => (Word8 -> Bool) -> Conduit ByteString m ByteString
-- splitOn pred =
--     loop []
--   where
--     loop acc = await >>= maybe (finish acc) (go acc)

--     finish acc =
--         let final = ByteString.concat $ reverse acc
--          in unless (ByteString.null final) (yield final)

--     go acc more =
--         case ByteString.uncons second of
--             Just (_, second') -> yield (ByteString.concat $ reverse $ first:acc) >> go [] second'
--             Nothing -> loop $ more:acc
--       where
--         (first, second) = ByteString.break pred more
