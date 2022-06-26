{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE Safe                #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RFP.Internal.Trigger (
    Trigger(..),
    discardTrigger,
    triggerBoth,
    filterTrigger,
    prependK
) where

    import           Control.Applicative                  (liftA2)
    import           Control.Arrow                        (Kleisli (..))
    import qualified Data.Functor.Contravariant           as CF
    import qualified Data.Functor.Contravariant.Divisible as Div
    import           Data.Void                            (absurd)

    newtype Trigger m a = Trigger { trigger :: a -> m () }

    instance CF.Contravariant (Trigger m) where
        contramap f m = Trigger $ trigger m . f

    instance Applicative m => Div.Divisible (Trigger m) where
        divide f trigB trigC = Trigger go
            where
                go a =
                    let (b, c) = f a in
                    liftA2
                        (\() () -> ())
                        (trigger trigB b)
                        (trigger trigC c)

        conquer = Trigger $ const (pure ())

    instance Applicative m => Div.Decidable (Trigger m) where
        lose f = Trigger $ absurd . f
        choose f trigB trigC = Trigger go
            where
                go a =
                    case f a of
                        Left b  -> trigger trigB b
                        Right c -> trigger trigC c


    -- | A trigger that does nothing when triggered.
    --
    -- Useful for situations where you need to have a trigger, but
    -- don't want to actually do anything in the trigger (for example,
    -- when you're doing a divide or a choose, but don't care about
    -- one branch).
    --
    discardTrigger :: Applicative m => Trigger m a
    discardTrigger = Trigger $ const (pure ())

    -- | Combine two triggers.
    --
    -- Creates a single combined trigger than triggers both input
    -- triggers.
    --
    triggerBoth :: Applicative m => Trigger m a -> Trigger m a -> Trigger m a
    triggerBoth trig1 trig2 = Trigger $
                                \a -> liftA2
                                        (\ () () -> ())
                                        (trigger trig1 a)
                                        (trigger trig2 a)


    -- | Filter a trigger.
    --
    -- Only fire the input trigger if the function given returns true.
    filterTrigger :: Applicative m => (a -> Bool) -> Trigger m a -> Trigger m a
    filterTrigger f trig = Trigger $
                            \a -> if (f a)
                                    then trigger trig a
                                    else pure ()

    prependK :: forall m a b . Monad m =>
                Kleisli m a b -> Trigger m b -> Trigger m a
    prependK k trig = Trigger go
        where
            go :: a -> m ()
            go a = do
                b <- runKleisli k a
                trigger trig b

