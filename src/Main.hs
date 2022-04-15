module Main where

import Control.Monad.State.Strict
import Counter
import Data.Dynamic
import Data.Map.Strict
import Data.Maybe
import Data.Proxy
import Data.Typeable
import Greet
import OOP
import Prelude hiding (lookup)

main :: IO ()
main = do
  putStrLn "<< Functional Object Oriented Programming >>"
  let world =
        ObjectWorld
          { next_oid = 1,
            classes = empty,
            population = empty
          }
  (_, world') <- flip runStateT world $ do
    oop'counter
    oop'greet
  return ()
