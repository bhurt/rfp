{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module RFP.Internal.Nonthreaded(
    Nonthreaded,
    isNonthreaded
) where

    import           Control.Applicative    (Alternative (..), Applicative (..))
    import           Control.Monad.IO.Class
    import           Control.Monad.Reader
    import           Data.IORef
    import           Data.Kind              (Type)
    import           Data.Semigroup         (Semigroup (..))
    import           Data.Typeable
    import           RFP.Internal.Behavior
    import           RFP.Internal.GetTime
    import           RFP.Internal.Hold
    import           RFP.Internal.PerformIO
    import           RFP.Internal.Runnable
    import           RFP.Internal.Trigger
    import           RFP.Internal.Utils

    newtype Nonthreaded a =
        Nonthreaded { getNonthreaded :: ReaderT (Env IORef) IO a }

    instance Functor Nonthreaded where
        fmap f =  Nonthreaded . fmap f . getNonthreaded
        a <$ f = Nonthreaded $ a <$ getNonthreaded f

    instance Applicative Nonthreaded where
        pure = Nonthreaded . pure 
        f <*> g = Nonthreaded $ (getNonthreaded f) <*> (getNonthreaded g)
        liftA2 f a b = Nonthreaded $ liftA2 f (getNonthreaded a) (getNonthreaded b)
        a *> b = Nonthreaded $ getNonthreaded a *> getNonthreaded b
        a <* b = Nonthreaded $ getNonthreaded a <* getNonthreaded b

    instance Monad Nonthreaded where
        return = pure
        x >>= f = Nonthreaded $ getNonthreaded x >>= (getNonthreaded . f)
        (>>) = (*>)

    instance MonadPlus Nonthreaded where
        mzero = Nonthreaded mzero
        mplus x y = Nonthreaded $ mplus (getNonthreaded x) (getNonthreaded y)

    instance Alternative Nonthreaded where
        empty = Nonthreaded empty
        f <|> g = Nonthreaded $ getNonthreaded f <|> getNonthreaded g
        some f = Nonthreaded $ some (getNonthreaded f)
        many f = Nonthreaded $ many (getNonthreaded f)

    instance Semigroup a => Semigroup (Nonthreaded a) where
        a <> b = Nonthreaded $ liftA2 (<>) (getNonthreaded a) (getNonthreaded b)
        sconcat xs = Nonthreaded $ sconcat <$> sequenceA (getNonthreaded <$> xs)
        stimes n x = Nonthreaded $ stimes n <$> getNonthreaded x

    instance Monoid a => Monoid (Nonthreaded a) where
        mempty = Nonthreaded $ pure mempty
        mappend = (<>)
        mconcat xs = Nonthreaded $ mconcat <$> sequenceA (getNonthreaded <$> xs)

    instance MonadFix Nonthreaded where
        mfix f = Nonthreaded $ mfix (getNonthreaded . f)

    instance Runnable Nonthreaded where
        runMoment f = liftIO $ do
            nowTime <- getTime
            acts :: [ IO () ] <- do
                var :: IORef [ IO () ] <- newIORef []
                runReaderT (getNonthreaded f) $ Env {
                    ios = var,
                    time = nowTime }
                readIORef var
            case acts of
                [] -> pure ()
                _ -> mapM_ id $ reverse acts

    instance Hold Nonthreaded where
        hold x = liftIO $ do
            var <- newIORef x
            pure $ (Behavior (Nonthreaded . lift $ readIORef var),
                    Trigger (Nonthreaded . lift . writeIORef var))

    instance PerformIO Nonthreaded where
        performIO =
            Trigger $
                \act ->
                    Nonthreaded $ do
                        var <- ios <$> ask
                        lift $ do
                            acts <- readIORef var
                            writeIORef var (act : acts)

    instance GetTime Nonthreaded where
        currentTime = Behavior $ Nonthreaded $ time <$> ask

    isNonthreaded :: forall (m :: Type -> Type) . Typeable m => Proxy m -> Bool
    isNonthreaded Proxy = case eqT :: Maybe (m :~: Nonthreaded) of
                            Just _  -> True
                            Nothing -> False

