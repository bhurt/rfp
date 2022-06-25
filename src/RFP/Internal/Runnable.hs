{-# LANGUAGE Safe                #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RFP.Internal.Runnable(
    Runnable(..)
) where

    import           Control.Monad.IO.Class (MonadIO)

    class Runnable f where
        runMoment :: forall m . MonadIO m => f () -> m ()

