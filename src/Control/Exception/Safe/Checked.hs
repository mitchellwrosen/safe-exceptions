{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE RoleAnnotations #-}
#endif

-- | Lightweight checked exceptions, based on
-- <https://www.well-typed.com/blog/2015/07/checked-exceptions/>.

module Control.Exception.Safe.Checked
    ( -- * Throwing
      Throws
    , ThrowsImpure
    , throw
    , impureThrow
      -- * Catching (with recovery)
    , catch
    , catchDeep

    , handle
    , handleDeep

    , try
    , tryDeep

      -- * Cleanup (no recovery)
    , withException
    ) where

import Control.DeepSeq (NFData)
import Control.Exception (Exception (..))
import qualified Control.Exception.Safe as Safe
import qualified Control.Monad.Catch as C
import Control.Monad.IO.Class (MonadIO)
import Data.Proxy (Proxy (..))

#if __GLASGOW_HASKELL__ >= 708
import Data.Coerce (coerce)
#else
import Unsafe.Coerce (unsafeCoerce)
#endif

-- | Like 'Safe.throw', but for checked exceptions.
throw :: (C.MonadThrow m, Exception e, Throws e) => e -> m a
throw = Safe.throw

-- | Like 'Safe.impureThrow', but for checked exceptions.
impureThrow :: (Exception e, ThrowsImpure e) => e -> a
impureThrow = Safe.impureThrow

-- | Like 'Safe.catch', but for checked exceptions.
catch :: (C.MonadCatch m, Exception e) => (Throws e => m a) -> (e -> m a) -> m a
catch = catch_
  where
    catch_ :: forall a e m. (C.MonadCatch m, Exception e)
           => (Throws e => m a) -> (e -> m a) -> m a
    catch_ = Safe.catch . unthrow (Proxy :: Proxy e)

-- | Like 'Safe.catchDeep', but for checked exceptions.
catchDeep :: (C.MonadCatch m, MonadIO m, Exception e, NFData a)
          => (ThrowsImpure e => m a) -> (e -> m a) -> m a
catchDeep = catchDeep_
  where
    catchDeep_ :: forall a e m. (C.MonadCatch m, MonadIO m, Exception e, NFData a)
               => (ThrowsImpure e => m a) -> (e -> m a) -> m a
    catchDeep_ = Safe.catchDeep . unthrowImpure (Proxy :: Proxy e)

-- | Like 'Safe.handle', but for checked exceptions.
handle :: (C.MonadCatch m, Exception e)
       => (e -> m a) -> (Throws e => m a) -> m a
handle f g = catch g f

-- | Like 'Safe.handleDeep', but for checked exceptions.
handleDeep :: (C.MonadCatch m, MonadIO m, Exception e, NFData a)
           => (e -> m a) -> (ThrowsImpure e => m a) -> m a
handleDeep f g = catchDeep g f

-- | Like 'Safe.try', but for checked exceptions.
try :: (C.MonadCatch m, Exception e, Throws e)
    => (Throws e => m a) -> m (Either e a)
try = try_
  where
    try_ :: forall a e m. (C.MonadCatch m, Exception e, Throws e)
         => (Throws e => m a) -> m (Either e a)
    try_ = Safe.try . unthrow (Proxy :: Proxy e)

-- | Like 'Safe.tryDeep', but for checked exceptions.
tryDeep :: (C.MonadCatch m, MonadIO m, Exception e, NFData a)
        => (ThrowsImpure e => m a) -> m (Either e a)
tryDeep = tryDeep_
  where
    tryDeep_ :: forall a e m. (C.MonadCatch m, MonadIO m, Exception e, NFData a)
             => (ThrowsImpure e => m a) -> m (Either e a)
    tryDeep_ = Safe.tryDeep . unthrowImpure (Proxy :: Proxy e)

-- | Like 'Safe.withException', but for checked exceptions.
withException :: (C.MonadMask m, Exception e) => (Throws e => m a) -> (e -> m b) -> m a
withException = withException_
  where
    withException_ :: forall a b e m. (C.MonadMask m, Exception e)
                   => (Throws e => m a) -> (e -> m b) -> m a
    withException_ = Safe.withException . unthrow (Proxy :: Proxy e)

--------------------------------------------------------------------------------
-- Throws/ThrowsImpure machinery

-- Unexported superclass of 'Throws' to prevent other instances.
class X e

-- | A @'Throws' e@ constraint indicates a computation may throw synchronous
-- exception @e@. Introduce a constraint with 'throw', and discharge it with
-- e.g. 'catch'.
--
-- You may ignore the @X@ superclass, it exists only to prevent additional
-- 'Throws' instances from being created.
class X e => Throws e

-- | A @'ThrowsImpure' e@ constraint indicates a computation may throw impure
-- exception @e@. Introduce a constraint with 'impureThrow', and discharge it
-- with e.g. 'catchDeep'.
--
-- You may ignore the @X@ superclass, it exists only to prevent additional
-- 'ThrowsImpure' instances from being created.
class X e => ThrowsImpure e

#if __GLASGOW_HASKELL__ >= 708
type role X representational
type role Throws representational
type role ThrowsImpure representational
#endif

newtype Wrap e a = Wrap { unWrap :: Throws e => a }
newtype WrapImpure e a = WrapImpure { unWrapImpure :: ThrowsImpure e => a }

newtype Catch a = Catch a

instance X (Catch a)
instance Throws (Catch a)
instance ThrowsImpure (Catch a)

coerceWrap :: Wrap e a -> Wrap (Catch e) a
#if __GLASGOW_HASKELL__ >= 708
coerceWrap = coerce
#else
coerceWrap = unsafeCoerce
#endif

coerceWrapImpure :: WrapImpure e a -> WrapImpure (Catch e) a
#if __GLASGOW_HASKELL__ >= 708
coerceWrapImpure = coerce
#else
coerceWrapImpure = unsafeCoerce
#endif

unthrow :: Proxy e -> (Throws e => a) -> a
unthrow _ = unWrap . coerceWrap . Wrap

unthrowImpure :: Proxy e -> (ThrowsImpure e => a) -> a
unthrowImpure _ = unWrapImpure . coerceWrapImpure . WrapImpure
