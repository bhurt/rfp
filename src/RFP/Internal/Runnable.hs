{-# LANGUAGE Safe                #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RFP.Internal.Runnable(
    Runnable(..)
) where

    import           Control.Monad.IO.Class (MonadIO)

    class Runnable m where
        runMoment :: forall dom . MonadIO dom => m () -> dom ()

