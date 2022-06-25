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

    class PerformIO t where
        performIO :: Trigger t (IO ())

    performTrigger :: forall a t .
                        (Runnable t
                        , PerformIO t)
                        => Trigger t a
                        -> Trigger t (IO a)
    performTrigger fini = contramap go performIO
        where
            go :: IO a -> IO ()
            go ioa = do
                a <- ioa
                runMoment $ trigger fini a
            {-# INLINE go #-}

    performTriggerOnce :: forall t m .
                            (Monad t
                            , Runnable t
                            , Hold t
                            , PerformIO t
                            , MonadIO m)
                            => Trigger t ()
                            -> m (Trigger t (IO ()))
    performTriggerOnce fini = do
            (g, upd) <- hold True
            pure . Trigger $ go g upd
        where
            go :: Behavior t Bool -> Trigger t Bool -> IO () -> t ()
            go g upd act = do
                t <- sample g
                if t
                then do
                    trigger upd False
                    trigger performIO $ wrap upd act
                else
                    pure ()

            wrap :: Trigger t Bool -> IO () -> IO ()
            wrap upd act = do
                act
                runMoment $ do
                    trigger upd True
                    trigger fini ()

