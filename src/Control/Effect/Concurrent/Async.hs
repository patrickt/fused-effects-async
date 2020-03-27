{-# LANGUAGE RankNTypes #-}

-- | Operations from "Control.Concurrent.Async" lifted into effectful contexts using 'Control.Effect.Lift.Lift'.
--
-- Note that the action parameters operate in the 'IO' monad, rather than in the invoking monad stack; this is
-- an unfortunate consequence of the fact that we cannot enforce that a given monad stack contains no monadic
-- state, without which we cannot guarantee that asynchronous actions will not pollute a monad stack's state.
module Control.Effect.Concurrent.Async
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

    -- ** Querying 'Async's
    wait,
    poll,
    waitCatch,
    C.asyncThreadId,
    cancel,
    uninterruptibleCancel,
    cancelWith,
    C.AsyncCancelled (..),

    -- ** STM operations
    waitSTM,
    pollSTM,
    waitCatchSTM,

    -- ** Waiting for multiple 'Async's
    waitAny,
    waitAnyCatch,
    waitAnyCancel,
    waitAnyCatchCancel,
    waitEither,
    waitEitherCatch,
    waitEitherCancel,
    waitEitherCatchCancel,
    waitEither_,
    waitBoth,

    -- ** Waiting for multiple 'Async's in STM
    waitAnySTM,
    waitAnyCatchSTM,
    waitEitherSTM,
    waitEitherCatchSTM,
    waitEitherSTM_,
    waitBothSTM,
  )
where

import           Control.Concurrent.Async (Async)
import qualified Control.Concurrent.Async as C
import           Control.Concurrent.STM (STM)
import           Control.Effect.Lift
import           Control.Exception (Exception, SomeException)

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
withAsync :: Has (Lift IO) sig m => IO a -> (Async a -> IO b) -> m b
withAsync act = sendM . C.withAsync act

-- | See @"Control.Concurrent.Async".'C.withAsyncBound'@.
withAsyncBound :: Has (Lift IO) sig m => IO a -> (Async a -> IO b) -> m b
withAsyncBound act = sendM . C.withAsyncBound act

-- | See @"Control.Concurrent.Async".'C.withAsyncOn'@.
withAsyncOn :: Has (Lift IO) sig m => Int -> IO a -> (Async a -> IO a) -> m a
withAsyncOn cpu act = sendM . C.withAsyncOn cpu act

-- | See @"Control.Concurrent.Async".'C.withAsyncWithUnmask'@.
withAsyncWithUnmask :: Has (Lift IO) sig m => ((forall b. IO b -> IO b) -> IO a) -> (Async a -> IO a) -> m a
withAsyncWithUnmask x act = sendM (C.withAsyncWithUnmask x act)

-- | See @"Control.Concurrent.Async".'C.withAsyncOnWithUnmask'@.
withAsyncOnWithUnmask :: Has (Lift IO) sig m => Int -> ((forall b. IO b -> IO b) -> IO a) -> (Async a -> IO a) -> m a
withAsyncOnWithUnmask cpu act go = sendM (C.withAsyncOnWithUnmask cpu act go)

-- | See @"Control.Concurrent.Async".'C.wait'@.
wait :: Has (Lift IO) sig m => Async a -> m a
wait = sendM . C.wait

-- | See @"Control.Concurrent.Async".'C.poll'@.
poll :: Async a -> IO (Maybe (Either SomeException a))
poll = sendM . C.poll

-- | See @"Control.Concurrent.Async".'C.waitCatch'@.
waitCatch :: Has (Lift IO) sig m => Async a -> m (Either SomeException a)
waitCatch = sendM . C.waitCatch

-- | See @"Control.Concurrent.Async".'C.cancel'@.
cancel :: Has (Lift IO) sig m => Async a -> m ()
cancel = sendM . C.cancel

-- | See @"Control.Concurrent.Async".'C.uninterruptibleCancel'@.
uninterruptibleCancel :: Has (Lift IO) sig m => Async a -> m ()
uninterruptibleCancel = sendM . C.uninterruptibleCancel

-- | See @"Control.Concurrent.Async".'C.cancelWith'@.
cancelWith :: (Has (Lift IO) sig m, Exception e) => Async a -> e -> m ()
cancelWith act = sendM . C.cancelWith act

-- | See @"Control.Concurrent.Async".'C.waitSTM'@.
waitSTM :: Has (Lift STM) sig m => Async a -> m a
waitSTM = sendM . C.waitSTM

-- | See @"Control.Concurrent.Async".'C.pollSTM'@.
pollSTM :: Has (Lift STM) sig m => Async a -> m (Maybe (Either SomeException a))
pollSTM = sendM . C.pollSTM

-- | See @"Control.Concurrent.Async".'C.waitCatchSTM'@.
waitCatchSTM :: Has (Lift STM) sig m => Async a -> m (Either SomeException a)
waitCatchSTM = sendM . C.waitCatchSTM

-- | See @"Control.Concurrent.Async".'C.waitAny'@.
waitAny :: Has (Lift IO) sig m => [Async a] -> m (Async a, a)
waitAny = sendM . C.waitAny

-- | See @"Control.Concurrent.Async".'C.waitAnyCatch'@.
waitAnyCatch :: Has (Lift IO) sig m => [Async a] -> m (Async a, Either SomeException a)
waitAnyCatch = sendM . C.waitAnyCatch

-- | See @"Control.Concurrent.Async".'C.waitAnyCancel'@.
waitAnyCancel :: Has (Lift IO) sig m => [Async a] -> m (Async a, a)
waitAnyCancel = sendM . C.waitAnyCancel

-- | See @"Control.Concurrent.Async".'C.waitAnyCatchCancel'@.
waitAnyCatchCancel :: Has (Lift IO) sig m => [Async a] -> m (Async a, Either SomeException a)
waitAnyCatchCancel = sendM . C.waitAnyCatchCancel

-- | See @"Control.Concurrent.Async".'C.waitEither'@.
waitEither :: Has (Lift IO) sig m => Async a -> Async b -> m (Either a b)
waitEither act = sendM . C.waitEither act

-- | See @"Control.Concurrent.Async".'C.waitEitherCatch'@.
waitEitherCatch :: Has (Lift IO) sig m => Async a -> Async b -> m (Either (Either SomeException a) (Either SomeException b))
waitEitherCatch go = sendM . C.waitEitherCatch go

-- | See @"Control.Concurrent.Async".'C.waitEitherCancel'@.
waitEitherCancel :: Has (Lift IO) sig m => Async a -> Async b -> m (Either a b)
waitEitherCancel go = sendM . C.waitEitherCancel go

-- | See @"Control.Concurrent.Async".'C.waitEitherCatchCancel'@.
waitEitherCatchCancel :: Has (Lift IO) sig m => Async a -> Async b -> m (Either (Either SomeException a) (Either SomeException b))
waitEitherCatchCancel go = sendM . C.waitEitherCatchCancel go

-- | See @"Control.Concurrent.Async".'C.waitEither'@.
waitEither_ :: Has (Lift IO) sig m => Async a -> Async b -> m ()
waitEither_ act = sendM . C.waitEither_ act

-- | See @"Control.Concurrent.Async".'C.waitBoth'@.
waitBoth :: Has (Lift IO) sig m => Async a -> Async b -> m (a, b)
waitBoth act = sendM . C.waitBoth act

-- | See @"Control.Concurrent.Async".'C.waitAnySTM'@.
waitAnySTM :: Has (Lift STM) sig m => [Async a] -> m (Async a, a)
waitAnySTM = sendM . C.waitAnySTM

-- | See @"Control.Concurrent.Async".'C.waitAnyCatchSTM'@.
waitAnyCatchSTM :: Has (Lift STM) sig m => [Async a] -> m (Async a, Either SomeException a)
waitAnyCatchSTM = sendM . C.waitAnyCatchSTM

-- | See @"Control.Concurrent.Async".'C.waitEitherSTM'@.
waitEitherSTM :: Has (Lift STM) sig m => Async a -> Async b -> m (Either a b)
waitEitherSTM act = sendM . C.waitEitherSTM act

-- | See @"Control.Concurrent.Async".'C.waitEitherCatchSTM'@.
waitEitherCatchSTM :: Has (Lift STM) sig m => Async a -> Async b -> m (Either (Either SomeException a) (Either SomeException b))
waitEitherCatchSTM act go = sendM (C.waitEitherCatchSTM act go)

-- | See @"Control.Concurrent.Async".'C.waitEitherSTM'@.
waitEitherSTM_ :: Has (Lift STM) sig m => Async a -> Async b -> m ()
waitEitherSTM_ act go = sendM (C.waitEitherSTM_ act go)

-- | See @"Control.Concurrent.Async".'C.waitBothSTM'@.
waitBothSTM :: Has (Lift STM) sig m => Async a -> Async b -> m (a, b)
waitBothSTM act go = sendM (C.waitBothSTM act go)
