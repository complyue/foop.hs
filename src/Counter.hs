module Counter where

import Control.Monad.State.Strict
import Data.Dynamic
import Data.Map.Strict
import Data.Maybe
import Data.Proxy
import Data.Typeable
import OOP
import Prelude hiding (lookup)

oop'counter :: OOM ()
oop'counter = do
  clsCounter <- defineClass "Counter" Nothing ctors mths

  counterRef <- newObject clsCounter [toDyn (7 :: Int)]

  invokeMethod counterRef "increase" [] typeInt

  result <- invokeMethod counterRef "increase" [] typeInt

  lift $ putStrLn $ "Done, result= " <> show (fromDynamic result :: Maybe Int)
  where
    ctors =
      fromList
        [ -- Counter(n:Int)
          ([typeInt], \[n] -> return $ fromList [("n", n)])
        ]

    mths =
      fromList
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
