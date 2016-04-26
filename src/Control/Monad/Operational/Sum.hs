{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

#if __GLASGOW_HASKELL__ >= 710
#else
{-# LANGUAGE OverlappingInstances #-}
#endif

-- | Sum of operational semantics.
module Control.Monad.Operational.Sum (
    -- * Interpreter
    ProgramSum,
    singletonSum,
    interpretSumWithMonad,
    -- * Newtypes
    Interpreter(..),
    FA(..), unFA,
    NS1(..), unNS1,
    -- * Data re-expoets from generics-sop
    NP(..),
    -- * Helpers
    IsElem,
    -- * Re-exports
    module Control.Monad.Operational,
    ) where

import Control.Monad.Operational
import Generics.SOP

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

type ProgramSum instrs = Program (NS1 instrs)

-- | Flipped apply.
newtype FA a f = FA (f a)
  deriving (Eq, Show)

unFA :: FA a f -> f a
unFA (FA fa) = fa

-- | n-sum of @* -> *@ types.
newtype NS1 (xs :: [* -> *]) a = NS1 (NS (FA a) xs)
  deriving (Eq, Show)

unNS1 :: NS1 xs a -> NS (FA a) xs
unNS1 (NS1 ns) = ns

-- | Natural transformation from @instr@ to @m@.
newtype Interpreter m instr = Interpreter { runInterpreter :: forall a. instr a -> m a }

-------------------------------------------------------------------------------
-- Interpreter
-------------------------------------------------------------------------------

-- | Generalisation of 'interpretWithMonad'.
interpretSumWithMonad
    :: forall instrs m b. Monad m
    => NP (Interpreter m) instrs
    -> Program (NS1 instrs) b
    -> m b
interpretSumWithMonad f = eval . view
  where
    eval :: forall a. ProgramView (NS1 instrs) a -> m a
    eval (Return a) = return a
    eval (m :>>= k) = apply f m >>= interpretSumWithMonad f . k

apply  :: NP (Interpreter m) instrs -> NS1 instrs b -> m b
apply np (NS1 ns) = apply' np ns
  where
    apply' :: NP (Interpreter m) instrs -> NS (FA b) instrs -> m b
    apply' (Interpreter f :* _)  (Z (FA x)) = f x
    apply' (_             :* fs) (S xs)     = apply' fs xs
    apply' _ _ = error "Control.Monad.Operational.Sum.apply: impossible happened"

singletonSum :: IsElem instr instrs => instr a -> ProgramSum instrs a
singletonSum = singleton . NS1 . inject . FA

-------------------------------------------------------------------------------
-- Helpers for implementing type classes
-------------------------------------------------------------------------------

-- | Can be used to implement type classes, for your @Instr@ and @MonadInstr@:
--
-- @
-- instance 'IsElem' Instr instrs => MonadInstr ('Program' ('NS1' instrs))
-- @

class IsElem x xs where
    inject :: f x -> NS f xs

instance
#if __GLASGOW_HASKELL__ >= 710
  {-# OVERLAPPING #-}
#endif
  IsElem x (x ': xs) where
    inject = Z

instance IsElem x xs => IsElem x (y ': xs) where
    inject = S . inject
