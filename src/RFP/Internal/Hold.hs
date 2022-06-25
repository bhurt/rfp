{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE Safe                 #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}

module RFP.Internal.Hold (
    Hold(..),
    updater
) where

    import           Control.Monad.IO.Class (MonadIO)
    import           RFP.Internal.Behavior
    import           RFP.Internal.Trigger

    class Hold t where
        hold :: forall m a . MonadIO m => a -> m (Behavior t a, Trigger t a)

    updater :: forall t m a . (Monad t, Hold t, MonadIO m)
                => a -> m (Behavior t a, Trigger t (a -> a))
    updater x = do
        (beh, trig) <- hold x
        let go :: (a -> a) -> t ()
            go f = do
                a <- sample beh
                trigger trig (f a)
            {-# INLINE go #-}
        pure (beh, Trigger go)

