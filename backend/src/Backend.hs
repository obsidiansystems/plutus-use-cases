{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Backend where

import Control.Exception
import Control.Monad.Identity
import Data.Dependent.Sum
import Data.Maybe
import Data.Pool
import Data.Proxy
import Data.Semigroup (First(..))
import Data.Vessel
import qualified Database.PostgreSQL.Simple as Pg
import Gargoyle.PostgreSQL.Connect
import Obelisk.Backend
import Obelisk.Route
import Rhyolite.Backend.App
-- import Rhyolite.Backend.DB
import Rhyolite.Backend.Listen

import Backend.Notification
import Common.Api
import Common.Route

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve ->
      withDb "db" $ \pool -> do
        (handleListen, finalizeServeDb) <- serveDbOverWebsockets
          pool
          (requestHandler)
          (\(nm :: DbNotification Notification) q -> fmap (fromMaybe emptyV) $ mapDecomposedV (notifyHandler nm) q)
          (QueryHandler $ \q -> fmap (fromMaybe emptyV) $ mapDecomposedV (queryHandler pool) q)
          vesselFromWire
          vesselPipeline
          -- (tracePipeline "==> Pacha" . vesselPipeline)
        flip finally finalizeServeDb $ serve $ \case
          BackendRoute_Listen :/ () -> handleListen
          _ -> return ()
  , _backend_routeEncoder = fullRouteEncoder
  }

requestHandler :: RequestHandler Api IO
requestHandler = RequestHandler $ \case
  Api -> return ()

queryHandler :: Pool Pg.Connection -> V Proxy -> IO (V Identity)
queryHandler _ v = buildV v $ \case
  Q -> \_ ->
    return $ IdentityV $ Identity $ First Nothing

notifyHandler :: DbNotification Notification -> V Proxy -> IO (V Identity)
notifyHandler dbNotification _ = case _dbNotification_message dbNotification of
  Notification_Test :=> Identity _ -> do
    let val = case _dbNotification_notificationType dbNotification of
          NotificationType_Delete -> Nothing
          NotificationType_Insert -> Just ()
          NotificationType_Update -> Just ()
    return $ singletonV Q $ IdentityV $ Identity $ First val
