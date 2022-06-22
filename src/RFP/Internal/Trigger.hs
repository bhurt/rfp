{-# LANGUAGE Safe                #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RFP.Internal.Trigger (
    Trigger(..)
) where

    import qualified Data.Functor.Contravariant           as CF
    import qualified Data.Functor.Contravariant.Divisible as Div
    import           Data.Void                            (absurd)

    newtype Trigger m a = Trigger { trigger :: a -> m () }

    instance CF.Contravariant (Trigger m) where
        contramap f t = Trigger $ trigger t . f

    instance Applicative m => Div.Divisible (Trigger m) where
        divide f trigB trigC = Trigger go
            where
                go a =
                    let (b, c) = f a in
                    (\() () -> ())
                        <$> trigger trigB b
                        <*> trigger trigC c

        conquer = Trigger $ const (pure ())

    instance Applicative m => Div.Decidable (Trigger m) where
        lose f = Trigger $ absurd . f
        choose f trigB trigC = Trigger go
            where
                go a =
                    case f a of
                        Left b  -> trigger trigB b
                        Right c -> trigger trigC c


