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
    Javascript,
    Static,

    -- ** Testing Models
    isThreaded,
    isJavascript,
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
    import           RFP.Internal.Javascript
    import           RFP.Internal.PerformIO
    import           RFP.Internal.Runnable
    import           RFP.Internal.Static
    import           RFP.Internal.Threaded
    import           RFP.Internal.Trigger

