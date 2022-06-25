{-# LANGUAGE Safe                #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RFP.Internal.Javascript(
    Javascript
) where

    import           Control.Applicative    (Alternative (..), Applicative (..))
    import           Control.Monad.IO.Class
    import           Control.Monad.Reader
    import           Data.IORef
    import           Data.Semigroup         (Semigroup (..))
    import           RFP.Internal.Behavior
    import           RFP.Internal.Hold
    import           RFP.Internal.PerformIO
    import           RFP.Internal.Runnable
    import           RFP.Internal.Trigger

    newtype Javascript a =
        Javascript { getJavascript :: ReaderT (IORef [ IO () ]) IO a }

    instance Functor Javascript where
        fmap f =  Javascript . fmap f . getJavascript
        a <$ f = Javascript $ a <$ getJavascript f

    instance Applicative Javascript where
        pure = Javascript . pure 
        f <*> g = Javascript $ (getJavascript f) <*> (getJavascript g)
        liftA2 f a b = Javascript $ liftA2 f (getJavascript a) (getJavascript b)
        a *> b = Javascript $ getJavascript a *> getJavascript b
        a <* b = Javascript $ getJavascript a <* getJavascript b

    instance Monad Javascript where
        return = pure
        x >>= f = Javascript $ getJavascript x >>= (getJavascript . f)
        (>>) = (*>)

    instance MonadPlus Javascript where
        mzero = Javascript mzero
        mplus x y = Javascript $ mplus (getJavascript x) (getJavascript y)

    instance Alternative Javascript where
        empty = Javascript empty
        f <|> g = Javascript $ getJavascript f <|> getJavascript g
        some f = Javascript $ some (getJavascript f)
        many f = Javascript $ many (getJavascript f)

    instance Semigroup a => Semigroup (Javascript a) where
        a <> b = Javascript $ liftA2 (<>) (getJavascript a) (getJavascript b)
        sconcat xs = Javascript $ sconcat <$> sequenceA (getJavascript <$> xs)
        stimes n x = Javascript $ stimes n <$> getJavascript x

    instance Monoid a => Monoid (Javascript a) where
        mempty = Javascript $ pure mempty
        mappend = (<>)
        mconcat xs = Javascript $ mconcat <$> sequenceA (getJavascript <$> xs)

    instance MonadFix Javascript where
        mfix f = Javascript $ mfix (getJavascript . f)

    instance Runnable Javascript where
        runMoment f = liftIO $ do
            ios :: [ IO () ] <- do
                var :: IORef [ IO () ] <- newIORef []
                runReaderT (getJavascript f) var
                readIORef var
            case ios of
                [] -> pure ()
                _ -> mapM_ id $ reverse ios

    instance Hold Javascript where
        hold x = liftIO $ do
            var <- newIORef x
            pure $ (Behavior (Javascript . lift $ readIORef var),
                    Trigger (Javascript . lift . writeIORef var))

    instance PerformIO Javascript where
        performIO = Trigger $ \act -> Javascript $ do
                                        var <- ask
                                        lift $ do
                                            ios <- readIORef var
                                            writeIORef var (act : ios)

