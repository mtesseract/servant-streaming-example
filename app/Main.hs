{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Main where

import           Conduit
import           Control.Exception                          hiding (Handler)
import           Control.Monad.Trans.Resource
import           Data.Aeson
import           Data.Aeson.Encoding
import           Data.ByteString                            (ByteString)
import qualified Data.ByteString                            as ByteString
import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy                       as ByteString.Lazy
import           Data.Conduit
import qualified Data.Conduit.Combinators                   as C
import qualified Data.Conduit.List                          as CL
import           Data.Maybe
import           Data.Monoid
import qualified Data.Vector                                as Vector
import           Network.HTTP.Media                         (MediaType)
import qualified Network.HTTP.Media                         as Media
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Conduit
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.API.ContentTypes
import           Servant.Server
import           Servant.Server.Internal.Router
import           Servant.Server.Internal.RoutingApplication

data GetStream a

class Streamable a where
  streamableToBuilder :: a -> Builder
  streamableCT :: Proxy a -> Maybe MediaType
  streamableCT _ = Nothing
  streamableDelimiter :: Proxy a -> Maybe Builder
  streamableDelimiter _ = Nothing

toBuilderDelimited :: forall a. Streamable a => Flush a -> [Flush Builder]
toBuilderDelimited (Chunk a) = [Chunk (streamableToBuilder a)]
toBuilderDelimited Flush = case streamableDelimiter (Proxy :: Proxy a) of
                             Just delim -> [Chunk delim, Flush]
                             Nothing    -> [Flush]

instance Streamable Value where
  streamableToBuilder = lazyByteString . encodingToLazyByteString . toEncoding
  streamableCT _ = Just ("application" Media.// "x-json-stream")
  streamableDelimiter _ = Just (lazyByteString "\n")

instance Streamable ByteString where
  streamableToBuilder = lazyByteString . ByteString.Lazy.fromStrict

instance Streamable a => HasServer (GetStream a) ctxt where
  type ServerT (GetStream a) m = m (Source (ResourceT IO) (Flush a))

  route Proxy _ctxt sub = leafRouter $
    \env req k ->
      bracket createInternalState
              closeInternalState
              (runAction sub env req k . mkResponse)
    where

      mkResponse :: InternalState
                 -> Source (ResourceT IO) (Flush a)
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

-- | This type defines the REST API for the server.
type ServiceAPI = "one" :> Get '[JSON] Int
   :<|> "ones" :> GetStream ByteString
   :<|> "hello" :> GetStream Value

-- | Define the Warp server.
serveAPI :: Server ServiceAPI
serveAPI =
  serveOne
  :<|> serveOnes
  :<|> serveHello

serveOne :: Handler Int
serveOne = return 1

serveOnes :: Handler (Source (ResourceT IO) (Flush ByteString))
serveOnes = return go
  where go = do
          yield (Chunk "1")
          yield Flush
          go

serveHello :: Handler (Source (ResourceT IO) (Flush Value))
serveHello = return go
  where go = do
          yield (Chunk (Array (Vector.replicate 10 (String "hello"))))
          yield Flush
          go

main :: IO ()
main = do
  run 8080 (serve (Proxy :: Proxy ServiceAPI) serveAPI)
  return ()

