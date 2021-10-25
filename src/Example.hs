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
        forM_ [1 .. 4] $ \i -> do                       -- 这段代码处于catchError之中，如果在这段代码中抛异常（throwError ),那么这段代码中产生的状态应该怎么处理。
          liftIO $ print $ "input number " ++ show i    -- 我想这里原本就应该存在两种可能。一种是产生的状态依旧存在，另一种是产生的状态被回滚（就好像这些产生状态的操作都失败了）。
          r <- liftIO getLine                           -- 于是在这里对StateC 和 ErrorC 两种的不同组合正是代表这两种情况。
          when (r /= show i) (throwError SControl)      -- 我们这里使用的就是状态回滚的语义,这里状态的操作具有原子性,因此可以重试整个操作。注意这里的IO操作一定不会也不能被回滚，因为IO一定在ErrorC外面。
          tell (show i) >> modify @Int (+ i)            -- 这里带有范围的副作用操作和单子栈不同组合顺序互相作用，产生了非常有趣的效果。
    )
    (\_ -> specInput)

runSpecInput :: IO (Either SControl (Int, (String, ())))
runSpecInput = runM @IO $ runError @SControl $ runState @Int 0 $ runWriter @String specInput
