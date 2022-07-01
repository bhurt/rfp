{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE Safe                 #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}

module RFP.Internal.Hold (
    Hold(..),
    updater,
    cache,
    cacheK,
    accumT,
    ValueNotifierKey,
    ValueNotifier(..),
    valueNotifier
) where

    import           Control.Arrow          (Kleisli (..))
    import           Control.Monad.IO.Class (MonadIO)
    import           Data.IntMap.Strict     (IntMap)
    import qualified Data.IntMap.Strict     as IntMap
    import           RFP.Internal.Behavior
    import           RFP.Internal.Trigger

    class Hold m where
        hold :: forall dom a .
                    MonadIO dom
                    => a
                    -> dom (Behavior m a, Trigger m a)

    updater :: forall m dom a . (Monad m, Hold m, MonadIO dom)
                => a -> dom (Behavior m a, Trigger m (a -> a))
    updater x = do
        (beh, trig) <- hold x
        let go :: (a -> a) -> m ()
            go f = do
                a <- sample beh
                fire trig (f a)
            {-# INLINE go #-}
        pure (beh, Trigger go)

    cache :: forall m dom a .
                (Monad m
                , Hold m
                , MonadIO dom)
                => a
                -> Behavior m a
                -> dom (Behavior m a, Trigger m ())
    cache x src = do
            (beh, upd) <- hold x
            pure (beh, Trigger (go upd))
        where
            go :: Trigger m a -> () -> m ()
            go upd = \ () -> do
                y <- sample src
                fire upd y

    cacheK :: forall m dom a b .
                (Monad m
                , Hold m
                , MonadIO dom)
                => b
                -> Kleisli m a b
                -> dom (Trigger m a, Behavior m b)
    cacheK x k = do
        (beh, upd) <- hold x
        pure $ (prependK k upd, beh)

    accumT :: forall m dom a .
                (Monad m
                , Hold m
                , MonadIO dom)
                => a
                -> Trigger m a
                -> dom (Trigger m (a -> a))
    accumT x trig = do
            (beh, upd) <- hold x
            pure . Trigger $ go beh upd
        where
            go :: Behavior m a -> Trigger m a -> (a -> a) -> m ()
            go beh upd = \f -> do
                a' <- sample beh
                let a = f a'
                fire upd a
                fire trig a

    newtype ValueNotifierKey = ValueNotifierKey Int

    data ValueNotifier m a = ValueNotifier {
        addListener :: Kleisli m (Trigger m a) ValueNotifierKey,
        removeListener :: Trigger m ValueNotifierKey,
        notifyListeners :: Trigger m a }

    data ValueNotifierState m a = ValueNotifierState {
        nextKey :: {-# UNPACK #-} !Int,
        currTriggers :: IntMap (Trigger m a) }

    valueNotifier :: forall m dom a .
                        (Monad m
                        , Hold m
                        , MonadIO dom)
                        => dom (ValueNotifier m a)
    valueNotifier = do
            (beh, upd) <- hold initialValueNotifierState
            pure $ ValueNotifier {
                        addListener = doAdd beh upd,
                        removeListener = doRem beh upd,
                        notifyListeners = doNote beh }
        where
            initialValueNotifierState :: ValueNotifierState m a
            initialValueNotifierState = ValueNotifierState {
                    nextKey = 0,
                    currTriggers = IntMap.empty
                }

            doAdd :: Behavior m (ValueNotifierState m a)
                        -> Trigger m (ValueNotifierState m a)
                        -> Kleisli m (Trigger m a) ValueNotifierKey
            doAdd beh upd = Kleisli $ \newTrig -> do
                st <- sample beh
                let key = nextKey st
                    trigs = currTriggers st
                    trigs2 = IntMap.insert key newTrig trigs
                    st2 = st {
                            nextKey = key + 1,
                            currTriggers = trigs2 }
                fire upd st2
                pure $ ValueNotifierKey key

            doRem :: Behavior m (ValueNotifierState m a)
                        -> Trigger m (ValueNotifierState m a)
                        -> Trigger m ValueNotifierKey
            doRem beh upd = Trigger $ \(ValueNotifierKey key) -> do
                st <- sample beh
                let trigs2 = IntMap.delete key (currTriggers st)
                    st2 = st { currTriggers = trigs2 }
                fire upd st2

            doNote :: Behavior m (ValueNotifierState m a)
                        -> Trigger m a
            doNote beh = Trigger $ \a -> do
                trigs <- currTriggers <$> sample beh
                mapM_ (\t -> fire t a) $ IntMap.elems trigs
    

