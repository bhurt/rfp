{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Safe              #-}

module Control.Monad.STM.Class(
    LiftSTM(..)
) where

    import           Control.Monad.STM   (STM)
    import qualified Control.Monad.Trans as Trans

    class LiftSTM m where
        liftSTM :: STM a -> m a

    instance LiftSTM STM where
        liftSTM = id

    instance (LiftSTM m, Monad m, Trans.MonadTrans t) => LiftSTM (t m) where
        liftSTM = Trans.lift . liftSTM


