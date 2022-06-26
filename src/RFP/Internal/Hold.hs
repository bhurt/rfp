{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE Safe                 #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}

module RFP.Internal.Hold (
    Hold(..),
    updater,
    cache
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


    cache :: forall m dom a .
                (Monad m
                , Hold m
                , MonadIO dom)
                => a
                -> Behavior m a
                -> dom (Behavior m a, Trigger m ())
    cache x src = do
            (beh, upd) <- hold x
            pure (beh, Trigger (go upd))
        where
            go :: Trigger m a -> () -> m ()
            go upd = \ () -> do
                y <- sample src
                trigger upd y

