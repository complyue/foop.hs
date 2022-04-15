module Main where

import Control.Monad.State.Strict
import Data.Dynamic
import Data.Map.Strict
import Data.Maybe
import Data.Proxy
import Data.Typeable
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
      (result, world') = runState oop_counter world

  putStrLn $ "Done, result= " <> show (fromDynamic result :: Maybe Int)
  where
    oop_counter :: OOM Dynamic
    oop_counter = do
      counterClass <- defineClass "Counter" Nothing (fromList ctors) (fromList mths)

      counterRef <- newObject counterClass [toDyn (7 :: Int)]

      invokeMethod counterRef "increase" [] typeInt

      invokeMethod counterRef "increase" [] typeInt

    ctors =
      [ -- Counter(n:Int)
        ([typeInt], \[n] -> return $ fromList [("n", n)])
      ]

    mths =
      [ -- increase():Int
        ( ("increase", ([], typeInt)),
          \obj [] -> do
            let this_states = obj_internals obj
                this_n = fromJust $ lookup "n" this_states
                n' :: Int = fromJust (fromDynamic this_n) + 1
                this_n' = toDyn n'
            let obj' = obj {obj_internals = insert "n" this_n' this_states}
            world <- get
            put world {population = insert (oid obj) obj' (population world)}
            return this_n'
        )
      ]

    typeInt = typeRep (Proxy @Int)
