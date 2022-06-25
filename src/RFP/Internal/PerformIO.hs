{-# LANGUAGE Safe                #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RFP.Internal.PerformIO (
    PerformIO(..),
    performTrigger,
    performTriggerOnce
) where

    import           Control.Monad.IO.Class     (MonadIO)
    import           Data.Functor.Contravariant (contramap)
    import           RFP.Internal.Behavior
    import           RFP.Internal.Hold
    import           RFP.Internal.Runnable
    import           RFP.Internal.Trigger

    class PerformIO m where
        performIO :: Trigger m (IO ())

    performTrigger :: forall a m .
                        (Runnable m
                        , PerformIO m)
                        => Trigger m a
                        -> Trigger m (IO a)
    performTrigger fini = contramap go performIO
        where
            go :: IO a -> IO ()
            go ioa = do
                a <- ioa
                runMoment $ trigger fini a
            {-# INLINE go #-}

    performTriggerOnce :: forall m dom .
                            (Monad m
                            , Runnable m
                            , Hold m
                            , PerformIO m
                            , MonadIO dom)
                            => Trigger m ()
                            -> dom (Trigger m (IO ()))
    performTriggerOnce fini = do
            (g, upd) <- hold True
            pure . Trigger $ go g upd
        where
            go :: Behavior m Bool -> Trigger m Bool -> IO () -> m ()
            go g upd act = do
                t <- sample g
                if t
                then do
                    trigger upd False
                    trigger performIO $ wrap upd act
                else
                    pure ()

            wrap :: Trigger m Bool -> IO () -> IO ()
            wrap upd act = do
                act
                runMoment $ do
                    trigger upd True
                    trigger fini ()

