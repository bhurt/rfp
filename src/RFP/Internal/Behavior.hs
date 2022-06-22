{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module RFP.Internal.Behavior (
    Behavior(..)
) where

    import           Control.Applicative (Alternative)
    import           Control.Monad       (MonadPlus)
    import           Control.Monad.Fix   (MonadFix)
    import           Control.Monad.Zip   (MonadZip)

    newtype Behavior m a = Behavior { sample :: m a }
        deriving (Functor, Applicative, Monad, MonadPlus, MonadFix,
                    Alternative, MonadZip, MonadFail)

