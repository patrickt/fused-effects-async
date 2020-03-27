{-# LANGUAGE RankNTypes #-}

module Control.Effect.Async
  ( -- * Asynchronous actions
    Async,

    -- * Spawning
    async,
    asyncBound,
    asyncOn,
    asyncWithUnmask,
    asyncOnWithUnmask,
  )
where

import Control.Concurrent.Async (Async)
import qualified Control.Concurrent.Async as C
import Control.Effect.Lift

-- | See @"Control.Concurrent.Async".'C.async'@.
async :: Has (Lift IO) sig m => IO a -> m (Async a)
async = sendM . C.async

-- | See @"Control.Concurrent.Async".'C.asyncBound'@.
asyncBound :: Has (Lift IO) sig m => IO a -> m (Async a)
asyncBound = sendM . C.asyncBound

-- | See @"Control.Concurrent.Async".'C.asyncOn'@.
asyncOn :: Has (Lift IO) sig m => Int -> IO a -> m (Async a)
asyncOn cpu = sendM . C.asyncOn cpu

-- | See @"Control.Concurrent.Async".'C.asyncWithUnmask'@.
asyncWithUnmask :: Has (Lift IO) sig m => ((forall b. IO b -> IO b) -> IO a) -> m (Async a)
asyncWithUnmask x = sendM (C.asyncWithUnmask x)

-- | See @"Control.Concurrent.Async".'C.asyncOnWithUnmask'@.
asyncOnWithUnmask :: Has (Lift IO) sig m => Int -> ((forall b. IO b -> IO b) -> IO a) -> m (Async a)
asyncOnWithUnmask cpu act = sendM (C.asyncOnWithUnmask cpu act)
