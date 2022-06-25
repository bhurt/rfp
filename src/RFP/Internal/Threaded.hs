{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE Safe                #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module RFP.Internal.Threaded(
    Threaded,
    isThreaded
) where

    import           Control.Applicative    (Alternative (..), Applicative (..))
    import           Control.Concurrent.STM
    import           Control.Monad.IO.Class
    import           Control.Monad.Reader
    import           Data.Kind              (Type)
    import           Data.Semigroup         (Semigroup (..))
    import           Data.Typeable
    import           RFP.Internal.Behavior
    import           RFP.Internal.Hold
    import           RFP.Internal.PerformIO
    import           RFP.Internal.Runnable
    import           RFP.Internal.Trigger

    newtype Threaded a =
        Threaded { getThreaded :: ReaderT (TVar [ IO () ]) STM a }

    instance Functor Threaded where
        fmap f =  Threaded . fmap f . getThreaded
        a <$ f = Threaded $ a <$ getThreaded f

    instance Applicative Threaded where
        pure = Threaded . pure 
        f <*> g = Threaded $ (getThreaded f) <*> (getThreaded g)
        liftA2 f a b = Threaded $ liftA2 f (getThreaded a) (getThreaded b)
        a *> b = Threaded $ getThreaded a *> getThreaded b
        a <* b = Threaded $ getThreaded a <* getThreaded b

    instance Monad Threaded where
        return = pure
        x >>= f = Threaded $ getThreaded x >>= (getThreaded . f)
        (>>) = (*>)

    instance MonadPlus Threaded where
        mzero = Threaded mzero
        mplus x y = Threaded $ mplus (getThreaded x) (getThreaded y)

    instance Alternative Threaded where
        empty = Threaded empty
        f <|> g = Threaded $ getThreaded f <|> getThreaded g
        some f = Threaded $ some (getThreaded f)
        many f = Threaded $ many (getThreaded f)

    instance Semigroup a => Semigroup (Threaded a) where
        a <> b = Threaded $ liftA2 (<>) (getThreaded a) (getThreaded b)
        sconcat xs = Threaded $ sconcat <$> sequenceA (getThreaded <$> xs)
        stimes n x = Threaded $ stimes n <$> getThreaded x

    instance Monoid a => Monoid (Threaded a) where
        mempty = Threaded $ pure mempty
        mappend = (<>)
        mconcat xs = Threaded $ mconcat <$> sequenceA (getThreaded <$> xs)

    instance MonadFix Threaded where
        mfix f = Threaded $ mfix (getThreaded . f)

    instance Runnable Threaded where
        runMoment f = liftIO $ do
            let go :: STM [ IO () ]
                go = do
                        var :: TVar [ IO () ] <- newTVar []
                        runReaderT (getThreaded f) var
                        readTVar var
            ios :: [ IO () ] <- atomically go
            case ios of
                [] -> pure ()
                _ -> mapM_ id $ reverse ios

    instance Hold Threaded where
        hold x = liftIO $ do
            var <- newTVarIO x
            pure $ (Behavior (Threaded . lift $ readTVar var),
                    Trigger (Threaded . lift . writeTVar var))

    instance PerformIO Threaded where
        performIO = Trigger $ \act -> Threaded $ do
                                        var <- ask
                                        lift $ do
                                            ios <- readTVar var
                                            writeTVar var (act : ios)

    isThreaded :: forall (m :: Type -> Type) . Typeable m => Proxy m -> Bool
    isThreaded Proxy = case eqT :: Maybe (m :~: Threaded) of
                            Just _  -> True
                            Nothing -> False

