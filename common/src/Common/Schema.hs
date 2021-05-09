{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- |

module Common.Schema where

import Control.Monad.Identity
import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.Int
import Database.Beam

data Db f = Db
  { _db_counter :: f (TableEntity CounterT)
  }
  deriving (Generic, Database be)

data CounterT f = Counter
  { _counter_id :: Columnar f Int32
  , _counter_amount :: Columnar f Int32
  }
  deriving (Generic)

instance Beamable CounterT

instance Table CounterT where
  newtype PrimaryKey CounterT f = CounterId { _counterId_id :: Columnar f Int32 }
    deriving (Generic)
  primaryKey = CounterId . _counter_id

instance Beamable (PrimaryKey CounterT)

type Counter = CounterT Identity

deriving instance Show Counter
deriving instance Eq Counter
deriving instance FromJSON Counter
deriving instance ToJSON Counter
