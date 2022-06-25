{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE Safe                #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RFP.Internal.Trigger (
    Trigger(..),
    discardTrigger
) where

    import           Control.Applicative                  (liftA2)
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
    discardTrigger :: Applicative m => Trigger m a
    discardTrigger = Trigger $ const (pure ())

