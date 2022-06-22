{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module RFP.Internal.Behavior (
    Behavior(..),
    attach
) where

    import           Control.Applicative  (Alternative)
    import           Control.Monad        (MonadPlus)
    import           Control.Monad.Fix    (MonadFix)
    import           Control.Monad.Zip    (MonadZip)
    import           RFP.Internal.Trigger

    newtype Behavior m a = Behavior { sample :: m a }
        deriving (Functor, Applicative, Monad, MonadPlus, MonadFix,
                    Alternative, MonadZip, MonadFail)

    attach :: forall m a b .
                Monad m
                => Behavior m a
                -> Trigger m (a, b)
                -> Trigger m b
    attach beha trigab = Trigger go
        where
            go :: b -> m ()
            go b = do
                a :: a <- sample beha
                trigger trigab (a, b)
            {-# INLINE go #-}


