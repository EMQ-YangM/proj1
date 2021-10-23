{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Mt where

newtype StateC s m a = StateC {runStateC :: s -> m (s, a)}
  deriving (Functor)

instance Monad m => Applicative (StateC s m) where
  pure a = StateC (\s -> pure (s, a))
  f <*> a = StateC $ \s -> do
    (s1, a1) <- runStateC f s
    (s2, a2) <- runStateC a s1
    return (s2, a1 a2)

instance Monad m => Monad (StateC s m) where
  a >>= b = StateC $ \s -> do
    (s1, a1) <- runStateC a s
    (s2, a2) <- runStateC (b a1) s1
    return (s2, a2)

get :: Monad m => StateC s m s
get = StateC (\s -> return (s, s))

put :: Monad m => s -> StateC s m ()
put s0 = StateC (\s -> return (s0, ()))

modify :: Monad m => (s -> s) -> StateC s m ()
modify f = StateC (\s -> return (f s, ()))

newtype ErrorC e m a = ErrorC {runErrorC :: m (Either e a)}
  deriving (Functor)

instance Monad m => Applicative (ErrorC e m) where
  pure a = ErrorC $ pure (Right a)
  f <*> a = ErrorC $ do
    f' <- runErrorC f
    a' <- runErrorC a
    pure (f' <*> a')

instance Monad m => Monad (ErrorC e m) where
  a >>= b = ErrorC $ do
    a' <- runErrorC a
    case a' of
      Left e   -> return (Left e)
      Right ra -> runErrorC $ b ra

type TE s a = StateC s (ErrorC String (StateC s (StateC s (StateC s IO)))) a

t :: TE Int ()
t = do
  put 100
  v <- get
  modify (+ 1)
  return ()

-- >>> rt
-- (0,(0,(0,Right (101,()))))
rt = (flip runStateC 0) $ (flip runStateC 0) $ (flip runStateC 0) $ runErrorC $ runStateC t 0
