{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}

module RFP.Internal.MonadHold (
    MonadHold(..),
    updater
) where

    import           Control.Concurrent.STM
    import           Control.Monad.IO.Class
    import           Control.Monad.Trans
    import           Data.IORef
    import           Data.Proxy
    import           RFP.Internal.Behavior
    import           RFP.Internal.Trigger

    class Monad n => MonadHold n where
        hold :: MonadIO m => a -> m (Behavior n a, Trigger n a)

    instance (MonadHold n, MonadTrans t, Monad (t n)) => MonadHold (t n) where
        hold a = fixup <$> hold a
            where
                fixup :: (Behavior n a, Trigger n a)
                            -> (Behavior (t n) a, Trigger (t n) a)
                fixup (Behavior b, Trigger t) =
                    (Behavior (lift b), Trigger (lift . t))
                {-# INLINE fixup #-}

    instance MonadHold STM where
        hold a = liftIO $ do
            var <- newTVarIO a
            pure (Behavior (readTVar var), Trigger (writeTVar var))

    instance MonadHold IO where
        hold a = liftIO $ do
            var <- newIORef a
            pure (Behavior (readIORef var), Trigger (writeIORef var))

    instance MonadHold Proxy where
        hold _ = pure (Behavior Proxy, Trigger (const Proxy))

    updater :: forall m n a . (MonadIO m, MonadHold n)
                => a -> m (Behavior n a, Trigger n (a -> a))
    updater x = do
        (beh, trig) <- hold x
        let go :: (a -> a) -> n ()
            go f = do
                a <- sample beh
                trigger trig (f a)
            {-# INLINE go #-}
        pure (beh, Trigger go)

