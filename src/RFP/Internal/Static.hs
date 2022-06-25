{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE Safe                #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module RFP.Internal.Static(
    Static,
    isStatic
) where

    import           Control.Applicative    (Alternative (..), Applicative (..))
    import           Control.Monad.Reader
    import           Data.Kind              (Type)
    import           Data.Proxy
    import           Data.Semigroup         (Semigroup (..))
    import           Data.Typeable
    import           RFP.Internal.Behavior
    import           RFP.Internal.Hold
    import           RFP.Internal.PerformIO
    import           RFP.Internal.Runnable
    import           RFP.Internal.Trigger

    newtype Static a = Static (Proxy a)

    instance Functor Static where
        fmap _ _ =  Static Proxy
        _ <$ _ = Static Proxy

    instance Applicative Static where
        pure _ = Static Proxy
        _ <*> _ = Static Proxy
        liftA2 _ _ _ = Static Proxy
        _ *> _ = Static Proxy
        _ <* _ = Static Proxy

    instance Monad Static where
        return = pure
        _ >>= _ = Static Proxy
        (>>) = (*>)

    instance MonadPlus Static where
        mzero = Static Proxy
        mplus _ _ = Static Proxy

    instance Alternative Static where
        empty = Static Proxy
        _ <|> _ = Static Proxy
        some _ = Static Proxy
        many _ = Static Proxy

    instance Semigroup (Static a) where
        _ <> _ = Static Proxy
        sconcat _ = Static Proxy
        stimes _ _ = Static Proxy

    instance Monoid (Static a) where
        mempty = Static Proxy
        mappend = (<>)
        mconcat _ = Static Proxy

    instance MonadFix Static where
        mfix _ = Static Proxy

    instance Runnable Static where
        runMoment _ = pure ()

    instance Hold Static where
        hold _ = 
            pure $ (Behavior (Static Proxy),
                    Trigger (const (Static Proxy)))

    instance PerformIO Static where
        performIO = Trigger $ const (Static Proxy)

    isStatic :: forall (m :: Type -> Type) . Typeable m => Proxy m -> Bool
    isStatic Proxy = case eqT :: Maybe (m :~: Static) of
                            Just _  -> True
                            Nothing -> False

