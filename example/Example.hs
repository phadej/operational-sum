{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}
{-# LANGUAGE FlexibleContexts #-}
module Main (main) where

import Control.Monad.Operational.Sum

data Foo a where Foo :: Foo ()
data Bar a where Bar :: Bar ()

foo :: IsElem Foo xs => ProgramSum xs ()
foo = singletonSum Foo

bar :: IsElem Bar xs => ProgramSum xs ()
bar = singletonSum Bar

example :: (IsElem Foo xs, IsElem Bar xs) => ProgramSum xs ()
example = foo >> bar

interpreters :: NP (Interpreter IO) '[Foo, Bar]
interpreters = Interpreter f :* Interpreter b :* Nil
  where
    f :: Foo a -> IO a
    f Foo = putStrLn "foo"

    b :: Bar a -> IO a
    b Bar = putStrLn "bar"

main :: IO ()
main = interpretSumWithMonad interpreters example
