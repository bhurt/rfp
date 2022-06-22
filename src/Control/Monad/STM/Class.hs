{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Safe              #-}

module Control.Monad.STM.Class(
    MonadSTM(..)
) where

    import           Control.Monad.STM   (STM)
    import qualified Control.Monad.Trans as Trans

    class MonadSTM m where
        liftSTM :: STM a -> m a

    instance MonadSTM STM where
        liftSTM = id

    instance (MonadSTM m, Monad m, Trans.MonadTrans t) => MonadSTM (t m) where
        liftSTM = Trans.lift . liftSTM


