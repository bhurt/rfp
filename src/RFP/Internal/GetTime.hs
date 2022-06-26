module RFP.Internal.GetTime (
    GetTime(..)
) where

    import           Data.Time             (UTCTime)
    import           RFP.Internal.Behavior

    class GetTime m where
        currentTime :: Behavior m UTCTime
