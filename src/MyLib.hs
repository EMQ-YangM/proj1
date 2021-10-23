{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

module MyLib where

import           Control.Algebra
import           Control.Carrier.Error.Either
import           Control.Carrier.Lift
import           Control.Carrier.Reader
import           Control.Carrier.State.Strict
import           Control.Effect.Optics
import           Control.Monad.IO.Class
import           Data.Kind
import           Optics

data Position = Position
  { _px :: Int,
    _py :: Int
  }
  deriving (Show)

data Person = Person
  { _name      :: String,
    _age       :: Int,
    _position  :: Position,
    _classname :: String,
    _teachers  :: [String]
  }
  deriving (Show)

makeLenses ''Position
makeLenses ''Person

initPerson = Person "yang" 23 (Position 10 30) "c4" ["a", "b"]

t ::
     (Has (Reader Int :+:
           State  Int :+:
           Error  Int :+:
           State Person) sig m,
      MonadIO m)
  => m ()
t = do
  r <- ask @Int
  if r >= 10
    then return ()
    else do
      p <- get @Person
      liftIO $ print ("first", r)
      position % px .= r
      position % py .= (r + r)
      liftIO $ print p
      local @Int (+ 1) t

r =
  runM @IO $
    runReader @Int 0 $
      runState @Int 0 $
        runState @Person initPerson $
          runError @Int t

data ErrorA = ErrorA

data ErrorB = ErrorB

data ErrorD = ErrorD

data HList :: [Type] -> Type where
  HNil :: HList '[]
  (:<>) :: a -> HList ls -> HList (a ': ls)

infixr 4 :<>

k :: HList [ErrorA, ErrorB, ErrorD]
k = ErrorA :<> ErrorB :<> ErrorD :<> HNil

data Exception (ls :: [Type]) b where
  Eleft :: HList ls -> Exception ls b
  Eright :: b -> Exception ls b
