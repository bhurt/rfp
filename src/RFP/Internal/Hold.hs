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

    class Hold m where
        hold :: forall dom a .
                    MonadIO dom
                    => a
                    -> dom (Behavior m a, Trigger m a)

    updater :: forall m dom a . (Monad m, Hold m, MonadIO dom)
                => a -> dom (Behavior m a, Trigger m (a -> a))
    updater x = do
        (beh, trig) <- hold x
        let go :: (a -> a) -> m ()
            go f = do
                a <- sample beh
                trigger trig (f a)
            {-# INLINE go #-}
        pure (beh, Trigger go)

