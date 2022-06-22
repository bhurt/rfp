{-# LANGUAGE ScopedTypeVariables #-}

module RFP (
    Trigger(..),
    Behavior(..),
    MonadHold(..),

    attach,
    updater
) where

    import           RFP.Internal.Behavior
    import           RFP.Internal.MonadHold
    import           RFP.Internal.Trigger
