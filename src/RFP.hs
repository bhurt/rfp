{-# LANGUAGE Safe                #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
--  Module:         RFP
--  Description:    Reactive Functional Programming- FRP with a twist
--  Copyright:      (c) 2022 Brian Hurt
--  License:        BSD-3
--  Maintainer:     bhurt42@gmail.com
--  Portability:    Safe
--
--  Reactive Functional Programming is a variation of Functional
--  Reactive Programming.  Except we replace events that produce
--  values with Triggers that consume them.  This allows a much
--  simpler (and more efficient) implementation.
--
module RFP (
    -- * Core types
    Trigger(..),
    Behavior(..),

    -- * Core type classes
    --
    -- We use typeclasses to allow multiple different implementations.
    --
    Runnable(..),
    Hold(..),
    PerformIO(..),

    -- * Moments
    --
    -- A moment is a monad that triggers (and sampling behaviors)
    -- can execute in.  By default the library supports three different
    -- types of moments for different situations.
    --
    Threaded,
    Nonthreaded,
    Static,

    -- ** Testing Moments
    isThreaded,
    isNonthreaded,
    isStatic

    -- * Other functions
    , appendK
    , apply
    , attach
    , discardTrigger
    , filterTrigger
    , gate
    , performTrigger
    , performTriggerOnce
    , prependK
    , triggerBoth
    , updater

) where

    import           RFP.Internal.Behavior
    import           RFP.Internal.Hold
    import           RFP.Internal.Nonthreaded
    import           RFP.Internal.PerformIO
    import           RFP.Internal.Runnable
    import           RFP.Internal.Static
    import           RFP.Internal.Threaded
    import           RFP.Internal.Trigger

