{-# LANGUAGE ScopedTypeVariables #-}

module RFP.Internal.Utils (
    Env(..),
    getTime
) where

    import           Data.IORef
    import           Data.Time        (UTCTime, getCurrentTime)
    import           System.IO.Unsafe (unsafePerformIO)

    data Env f = Env {
        ios :: f [ IO () ],
        time :: UTCTime
    }

    getTime :: IO UTCTime
    getTime = do
        ref :: IORef (Maybe UTCTime) <- newIORef Nothing
        pure $ unsafePerformIO $ do
            r <- readIORef ref
            case r of
                Just t  -> pure t
                Nothing -> do
                    t <- getCurrentTime
                    writeIORef ref (Just t)
                    pure t


