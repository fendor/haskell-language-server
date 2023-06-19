{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Test.Hls.Types where

import           Control.Monad.IO.Class
import qualified Control.Monad.Reader       as Reader
import           Control.Monad.Reader.Class
import           Control.Monad.Trans

newtype TestT r m a = TestT {runTestT :: Reader.ReaderT r m a}
  deriving
    ( Monad
    , Applicative
    , Functor
    , MonadIO
    , MonadFail
    )

instance Monad m => MonadReader r (TestT r m) where
  ask = TestT Reader.ask
  local f act = TestT (Reader.local f (runTestT act))

instance MonadTrans (TestT r) where
  lift a = TestT (lift a)
