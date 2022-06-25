{-# LANGUAGE Safe                #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RFP (
    -- * Core types
    Trigger(..),
    Behavior(..),

    -- * Core type classes
    Runnable(..),
    Hold(..),
    PerformIO(..),

    -- * Moments
    Threaded,
    Nonthreaded,
    Static,

    -- ** Testing Moments
    isThreaded,
    isNonthreaded,
    isStatic,

    -- * Other functions
    attach,
    updater,
    discardTrigger,
    gate,
    performTrigger,
    performTriggerOnce
) where

    import           RFP.Internal.Behavior
    import           RFP.Internal.Hold
    import           RFP.Internal.Nonthreaded
    import           RFP.Internal.PerformIO
    import           RFP.Internal.Runnable
    import           RFP.Internal.Static
    import           RFP.Internal.Threaded
    import           RFP.Internal.Trigger

