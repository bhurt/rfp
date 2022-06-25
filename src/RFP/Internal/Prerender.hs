{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE Safe                #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module RFP.Internal.Prerender(
    Prerender,
    isPrerender
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

    newtype Prerender a = Prerender (Proxy a)

    instance Functor Prerender where
        fmap _ _ =  Prerender Proxy
        _ <$ _ = Prerender Proxy

    instance Applicative Prerender where
        pure _ = Prerender Proxy
        _ <*> _ = Prerender Proxy
        liftA2 _ _ _ = Prerender Proxy
        _ *> _ = Prerender Proxy
        _ <* _ = Prerender Proxy

    instance Monad Prerender where
        return = pure
        _ >>= _ = Prerender Proxy
        (>>) = (*>)

    instance MonadPlus Prerender where
        mzero = Prerender Proxy
        mplus _ _ = Prerender Proxy

    instance Alternative Prerender where
        empty = Prerender Proxy
        _ <|> _ = Prerender Proxy
        some _ = Prerender Proxy
        many _ = Prerender Proxy

    instance Semigroup (Prerender a) where
        _ <> _ = Prerender Proxy
        sconcat _ = Prerender Proxy
        stimes _ _ = Prerender Proxy

    instance Monoid (Prerender a) where
        mempty = Prerender Proxy
        mappend = (<>)
        mconcat _ = Prerender Proxy

    instance MonadFix Prerender where
        mfix _ = Prerender Proxy

    instance Runnable Prerender where
        runMoment _ = pure ()

    instance Hold Prerender where
        hold _ = 
            pure $ (Behavior (Prerender Proxy),
                    Trigger (const (Prerender Proxy)))

    instance PerformIO Prerender where
        performIO = Trigger $ const (Prerender Proxy)

    isPrerender :: forall (t :: Type -> Type) . Typeable t => Proxy t -> Bool
    isPrerender Proxy = case eqT :: Maybe (t :~: Prerender) of
                            Just _  -> True
                            Nothing -> False

