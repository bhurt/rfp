{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE Safe                 #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}

module RFP.Internal.Hold (
    Hold(..),
    updater,
    cache,
    accumT
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

    accumT :: forall m dom a .
                (Monad m
                , Hold m
                , MonadIO dom)
                => a
                -> Trigger m a
                -> dom (Trigger m (a -> a))
    accumT x trig = do
            (beh, upd) <- hold x
            pure . Trigger $ go beh upd
        where
            go :: Behavior m a -> Trigger m a -> (a -> a) -> m ()
            go beh upd = \f -> do
                a' <- sample beh
                let a = f a'
                trigger upd a
                trigger trig a

