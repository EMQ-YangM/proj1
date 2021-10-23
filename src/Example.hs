{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}

module Example where

import           Control.Algebra
import           Control.Carrier.Error.Either
import           Control.Carrier.Lift
import           Control.Carrier.State.Strict
import           Control.Carrier.Writer.Strict
import qualified Control.Effect.Exception as E
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Kind
import qualified GHC.IO.Exception as IOE

foo :: (Has (State Int :+: Writer String :+: Error Int) sig m, MonadIO m) => m ()
foo = do
  catchError @Int
    ( do
        put @Int 1 -- update state
        tell "1"
        tell "2"
        tell "3"
        tell "4"
        throwError @Int 1 -- thow error
    )
    (\_ -> get @Int >>= liftIO . print)

-- >>> runFoo1
-- ("1234",(1,Right ()))
runFoo1 = runM @IO $ runWriter @String $ runState @Int 0 $ runError @Int foo

-- >>> runFoo2
-- Right ("",(0,()))
runFoo2 = runM @IO $ runError @Int $ runWriter @String $ runState @Int 0 foo

-- >>>  runFoo3
-- ("1234",Right (0,()))
runFoo3 = runM @IO $ runWriter @String $ runError @Int $ runState @Int 0 foo

-- >>>  runFoo4
-- (1,Right ("",()))
runFoo4 = runM @IO $ runState @Int 0 $ runError @Int $ runWriter @String foo

-- think 1, runState . runError

-- think 2, runError . runState

discardsState1 :: IO (Int, (String, ()))
discardsState1 =
  runM @IO $
    runState @Int 0 $
      runWriter @String
        ( do
            put @Int 20
            tell "happend 1 "
            tell " happend 2"
            E.catch
              ( E.catch
                  (tell " not output" >> E.throwIO (IOE.userError "urk"))
                  (\(e :: E.SomeException) -> tell " no output" >> E.throwIO e)
              )
              (\(_ :: IOException) -> pure ())
        )

discardsState :: IO Char
discardsState =
  execState
    'a'
    ( do
        E.catch
          ( E.catch
              ( do
                  put 'b'
                  E.throwIO (IOE.userError "urk")
              )
              ( \(e :: E.IOException) -> do
                  put @Char 'z'
                  E.throwIO e
              )
          )
          (\(_ :: IOException) -> pure ())
    )

discardsState0 :: IO Char
discardsState0 =
  execState
    'a'
    ( (put 'b' >> E.throwIO (IOE.userError "urk") )
        `E.catch` (\(_ :: IOException) -> pure ())
    )

tf = run $ runState @Int 0 $ runError @String undefined

data SControl = SControl deriving (Show)

specInput :: (Has (Error SControl :+: State Int :+: Writer String) sig m, MonadIO m) => m ()
specInput = do
  catchError @SControl
    ( do
        forM_ [1 .. 4] $ \i -> do
          liftIO $ print $ "input number " ++ show i
          r <- liftIO getLine
          when (r /= show i) (throwError SControl)
          tell (show i ++ " - ") >> modify @Int (+ i)
    )
    (const specInput)

runSpecInput :: IO (Either SControl (Int, (String, ())))
runSpecInput = runM @IO $ runError @SControl $ runState @Int 0 $ runWriter @String specInput
