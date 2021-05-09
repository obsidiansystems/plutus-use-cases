{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Backend where

import Control.Exception
import Control.Monad.Identity
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Dependent.Sum
import Data.Maybe
import Data.Pool
import Data.Proxy
import Data.Semigroup (First(..))
import Data.Vessel
import Database.Beam (MonadBeam)
import Database.Beam.Backend.SQL.BeamExtensions
import Database.Beam.Postgres
import Database.Beam.Query
import qualified Database.PostgreSQL.Simple as Pg
import Gargoyle.PostgreSQL.Connect
import Obelisk.Backend
import Obelisk.Route
import Rhyolite.Backend.App
import Rhyolite.Backend.DB
import Rhyolite.Backend.DB.Serializable
import Rhyolite.Backend.Listen

import Backend.Notification
import Backend.Schema
import Common.Api
import Common.Route
import Common.Schema

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve ->
      withDb "db" $ \pool -> do
        withResource pool runMigrations
        withResource pool $ \conn -> runBeamPostgres conn ensureCounterExists
        (handleListen, finalizeServeDb) <- serveDbOverWebsockets
          pool
          (requestHandler pool)
          (\(nm :: DbNotification Notification) q -> fmap (fromMaybe emptyV) $ mapDecomposedV (notifyHandler nm) q)
          (QueryHandler $ \q -> fmap (fromMaybe emptyV) $ mapDecomposedV (queryHandler pool) q)
          vesselFromWire
          vesselPipeline -- (tracePipeline "==> " . vesselPipeline)
        flip finally finalizeServeDb $ serve $ \case
          BackendRoute_Listen :/ () -> handleListen
          _ -> return ()
  , _backend_routeEncoder = fullRouteEncoder
  }

requestHandler :: Pool Pg.Connection -> RequestHandler Api IO
requestHandler pool = RequestHandler $ \case
  Api_IncrementCounter -> runNoLoggingT $ runDb (Identity pool) $ do
    rows <- runBeamSerializable $ do
      runUpdateReturningList $ update (_db_counter db)
        (\counter -> _counter_amount counter <-. current_ (_counter_amount counter) + val_ 1)
        (\counter -> _counter_id counter ==. val_ 0)
    mapM_ (notify NotificationType_Update Notification_Counter . _counter_amount) rows

notifyHandler :: DbNotification Notification -> V Proxy -> IO (V Identity)
notifyHandler dbNotification _ = case _dbNotification_message dbNotification of
  Notification_Counter :=> Identity n -> do
    let val = case _dbNotification_notificationType dbNotification of
          NotificationType_Delete -> Nothing
          NotificationType_Insert -> Just n
          NotificationType_Update -> Just n
    return $ singletonV Q_Counter $ IdentityV $ Identity $ First val

queryHandler :: Pool Pg.Connection -> V Proxy -> IO (V Identity)
queryHandler pool v = buildV v $ \case
  Q_Counter -> \_ -> runNoLoggingT $ runDb (Identity pool) $ runBeamSerializable $ do
    counter <- runSelectReturningOne $ lookup_ (_db_counter db) (CounterId 0)
    return $ IdentityV $ Identity $ First $ _counter_amount <$> counter

ensureCounterExists :: MonadBeam Postgres m => m ()
ensureCounterExists = do
  runInsert $ insertOnConflict (_db_counter db) (insertValues [(Counter 0 0)])
    (conflictingFields _counter_id)
    onConflictDoNothing

-- | Run a 'MonadBeam' action inside a 'Serializable' transaction. This ensures only safe
-- actions happen inside the 'Serializable'
runBeamSerializable :: (forall m. (MonadBeam Postgres m, MonadBeamInsertReturning Postgres m, MonadBeamUpdateReturning Postgres m, MonadBeamDeleteReturning Postgres m) => m a) -> Serializable a
runBeamSerializable action = unsafeMkSerializable $ liftIO . flip runBeamPostgres action =<< ask
