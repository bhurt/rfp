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

    newtype Trigger t a = Trigger { trigger :: a -> t () }

    instance CF.Contravariant (Trigger t) where
        contramap f t = Trigger $ trigger t . f

    instance Applicative t => Div.Divisible (Trigger t) where
        divide f trigB trigC = Trigger go
            where
                go a =
                    let (b, c) = f a in
                    liftA2
                        (\() () -> ())
                        (trigger trigB b)
                        (trigger trigC c)

        conquer = Trigger $ const (pure ())

    instance Applicative t => Div.Decidable (Trigger t) where
        lose f = Trigger $ absurd . f
        choose f trigB trigC = Trigger go
            where
                go a =
                    case f a of
                        Left b  -> trigger trigB b
                        Right c -> trigger trigC c


    discardTrigger :: Applicative t => Trigger t a
    discardTrigger = Trigger $ const (pure ())

