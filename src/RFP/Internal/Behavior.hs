{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE Safe                #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RFP.Internal.Behavior (
    Behavior(..),
    attach,
    gate
) where

    import           Control.Applicative  (Alternative (..), Applicative (..))
    import           Control.Monad        (MonadPlus (..))
    import           Control.Monad.Zip    (MonadZip (..))
    import           Data.Semigroup       (Semigroup (..))
    import           RFP.Internal.Trigger

    newtype Behavior t a = Behavior { sample :: t a }

    instance Functor t => Functor (Behavior t) where
        fmap f =  Behavior . fmap f . sample
        a <$ f = Behavior $ a <$ sample f

    instance Applicative t => Applicative (Behavior t) where
        pure = Behavior . pure 
        f <*> g = Behavior $ (sample f) <*> (sample g)
        liftA2 f a b = Behavior $ liftA2 f (sample a) (sample b)
        a *> b = Behavior $ sample a *> sample b
        a <* b = Behavior $ sample a <* sample b

    instance Monad t => Monad (Behavior t) where
        return = pure
        x >>= f = Behavior $ sample x >>= (sample . f)
        (>>) = (*>)

    instance MonadPlus t => MonadPlus (Behavior t) where
        mzero = Behavior mzero
        mplus x y = Behavior $ mplus (sample x) (sample y)

    instance Alternative t => Alternative (Behavior t) where
        empty = Behavior empty
        f <|> g = Behavior $ sample f <|> sample g
        some f = Behavior $ some (sample f)
        many f = Behavior $ many (sample f)

    instance MonadZip t => MonadZip (Behavior t) where
        mzip a b = Behavior $ mzip (sample a) (sample b)
        mzipWith f a b = Behavior $ mzipWith f (sample a) (sample b)
        munzip ab = let (a, b) = munzip (sample ab) in
                        (Behavior a, Behavior b)

    instance MonadFail t => MonadFail (Behavior t) where
        fail s = Behavior $ fail s

    instance (Applicative t, Semigroup a) => Semigroup (Behavior t a) where
        a <> b = Behavior $ liftA2 (<>) (sample a) (sample b)
        sconcat xs = Behavior $ sconcat <$> sequenceA (sample <$> xs)
        stimes n x = Behavior $ stimes n <$> sample x

    instance (Applicative t, Monoid a) => Monoid (Behavior t a) where
        mempty = Behavior $ pure mempty
        mappend = (<>)
        mconcat xs = Behavior $ mconcat <$> sequenceA (sample <$> xs)

    attach :: forall t a b .
                Monad t
                => Behavior t a
                -> Trigger t (a, b)
                -> Trigger t b
    attach beha trigab = Trigger go
        where
            go :: b -> t ()
            go b = do
                a :: a <- sample beha
                trigger trigab (a, b)
            {-# INLINE go #-}


    gate :: forall t a
            . Monad t
            => Behavior t Bool
            -> Trigger t a
            -> Trigger t a
    gate b t = Trigger go
        where
            go :: a -> t ()
            go a = do
                x <- sample b
                if x
                then trigger t a
                else pure ()
            {-# INLINE go #-}


