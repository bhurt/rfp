{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE Safe                #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RFP.Internal.Behavior (
    Behavior(..),
    attach,
    gate,
    apply,
    appendK
) where

    import           Control.Applicative  (Alternative (..), Applicative (..))
    import           Control.Arrow        (Kleisli (..))
    import           Control.Monad        (MonadPlus (..))
    import           Control.Monad.Fix    (MonadFix (..))
    import           Control.Monad.Zip    (MonadZip (..))
    import           Data.Semigroup       (Semigroup (..))
    import           RFP.Internal.Trigger

    newtype Behavior m a = Behavior { sample :: m a }

    instance Functor m => Functor (Behavior m) where
        fmap f =  Behavior . fmap f . sample
        a <$ f = Behavior $ a <$ sample f

    instance Applicative m => Applicative (Behavior m) where
        pure = Behavior . pure 
        f <*> g = Behavior $ (sample f) <*> (sample g)
        liftA2 f a b = Behavior $ liftA2 f (sample a) (sample b)
        a *> b = Behavior $ sample a *> sample b
        a <* b = Behavior $ sample a <* sample b

    instance Monad m => Monad (Behavior m) where
        return = pure
        x >>= f = Behavior $ sample x >>= (sample . f)
        (>>) = (*>)

    instance MonadPlus m => MonadPlus (Behavior m) where
        mzero = Behavior mzero
        mplus x y = Behavior $ mplus (sample x) (sample y)

    instance Alternative m => Alternative (Behavior m) where
        empty = Behavior empty
        f <|> g = Behavior $ sample f <|> sample g
        some f = Behavior $ some (sample f)
        many f = Behavior $ many (sample f)

    instance MonadZip m => MonadZip (Behavior m) where
        mzip a b = Behavior $ mzip (sample a) (sample b)
        mzipWith f a b = Behavior $ mzipWith f (sample a) (sample b)
        munzip ab = let (a, b) = munzip (sample ab) in
                        (Behavior a, Behavior b)

    instance MonadFail m => MonadFail (Behavior m) where
        fail s = Behavior $ fail s

    instance (Applicative m, Semigroup a) => Semigroup (Behavior m a) where
        a <> b = Behavior $ liftA2 (<>) (sample a) (sample b)
        sconcat xs = Behavior $ sconcat <$> sequenceA (sample <$> xs)
        stimes n x = Behavior $ stimes n <$> sample x

    instance (Applicative m, Monoid a) => Monoid (Behavior m a) where
        mempty = Behavior $ pure mempty
        mappend = (<>)
        mconcat xs = Behavior $ mconcat <$> sequenceA (sample <$> xs)

    instance MonadFix m => MonadFix (Behavior m) where
        mfix f = Behavior $ mfix (sample . f)


    attach :: forall m a b .
                Monad m
                => Behavior m a
                -> Trigger m (a, b)
                -> Trigger m b
    attach beha trigab = Trigger go
        where
            go :: b -> m ()
            go b = do
                a :: a <- sample beha
                trigger trigab (a, b)
            {-# INLINE go #-}


    gate :: forall m a
            . Monad m
            => Behavior m Bool
            -> Trigger m a
            -> Trigger m a
    gate b m = Trigger go
        where
            go :: a -> m ()
            go a = do
                x <- sample b
                if x
                then trigger m a
                else pure ()
            {-# INLINE go #-}

    apply :: forall m a b .
                Monad m
                => Behavior m (a -> b)
                -> Trigger m b
                -> Trigger m a
    apply beh out = Trigger go
        where
            go :: a -> m ()
            go a = do
                f <- sample beh
                trigger out (f a)

    appendK :: forall m a b .
                Monad m
                => Behavior m a
                -> Kleisli m a b
                -> Behavior m b
    appendK beh k = Behavior go
        where
            go :: m b
            go = do
                a <- sample beh
                runKleisli k a

