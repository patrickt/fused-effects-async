{-# LANGUAGE RankNTypes #-}

module Control.Effect.Async
  ( -- * Asynchronous actions
    Async,

    -- ** Spawning
    async,
    asyncBound,
    asyncOn,
    asyncWithUnmask,
    asyncOnWithUnmask,

    -- ** Spawning with automatic 'cancel'ation
    withAsync,
    withAsyncBound,
    withAsyncOn,
    withAsyncWithUnmask,
    withAsyncOnWithUnmask,
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

-- | See @"Control.Concurrent.Async".'C.withAsync'@.
withAsync :: Has (Lift IO) sig m => IO a -> m (Async a)
withAsync = sendM . C.withAsync

-- | See @"Control.Concurrent.Async".'C.withAsyncBound'@.
withAsyncBound :: Has (Lift IO) sig m => IO a -> m (Async a)
withAsyncBound = sendM . C.withAsyncBound

-- | See @"Control.Concurrent.Async".'C.withAsyncOn'@.
withAsyncOn :: Has (Lift IO) sig m => Int -> IO a -> m (Async a)
withAsyncOn cpu = sendM . C.withAsyncOn cpu

-- | See @"Control.Concurrent.Async".'C.withAsyncWithUnmask'@.
withAsyncWithUnmask :: Has (Lift IO) sig m => ((forall b. IO b -> IO b) -> IO a) -> m (Async a)
withAsyncWithUnmask x = sendM (C.withAsyncWithUnmask x)

-- | See @"Control.Concurrent.Async".'C.withAsyncOnWithUnmask'@.
withAsyncOnWithUnmask :: Has (Lift IO) sig m => Int -> ((forall b. IO b -> IO b) -> IO a) -> m (Async a)
withAsyncOnWithUnmask cpu act = sendM (C.withAsyncOnWithUnmask cpu act)
