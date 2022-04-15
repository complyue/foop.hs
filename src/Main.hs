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
  (_, world') <- flip runStateT world $ do
    oop'counter
    oop'greet
  return ()

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

oop'greet :: OOM ()
oop'greet = void $ do
  clsAnimal <- defineClass "Animal" Nothing ctors mthsAnimal
  clsSpider <- defineClass "Spider" (Just "Animal") ctors empty
  clsCat <- defineClass "Cat" (Just "Animal") ctors mthsCat
  clsDog <- defineClass "Dog" (Just "Animal") ctors mthsDog
  clsPeople <- defineClass "People" (Just "Animal") ctors mthsPeople

  newObject clsSpider [] >>= \objRef ->
    invokeMethod objRef "greet" [you] typeVoid
  newObject clsCat [] >>= \objRef ->
    invokeMethod objRef "greet" [you] typeVoid
  newObject clsDog [] >>= \objRef ->
    invokeMethod objRef "greet" [you] typeVoid
  newObject clsPeople [] >>= \objRef ->
    invokeMethod objRef "greet" [you] typeVoid
  where
    ctors =
      fromList
        [ -- do-nothing constructor
          ([], \[] -> return empty)
        ]

    mthsAnimal =
      fromList
        [ -- greet(mate:str)
          ( ("greet", ([typeString], typeVoid)),
            \obj [mate] -> do
              lift $
                putStrLn $
                  class_name (obj_class obj) <> " remains silent facing "
                    <> (fromJust (fromDynamic mate) :: String)
                    <> "."
              return valueVoid
          )
        ]
    mthsCat =
      fromList
        [ -- greet(mate:str)
          ( ("greet", ([typeString], typeVoid)),
            \obj [mate] -> do
              lift $ putStrLn "Meow, meow!"
              return valueVoid
          )
        ]
    mthsDog =
      fromList
        [ -- greet(mate:str)
          ( ("greet", ([typeString], typeVoid)),
            \obj [mate] -> do
              lift $ putStrLn "Woof, woof!"
              return valueVoid
          )
        ]
    mthsPeople =
      fromList
        [ -- greet(mate:str)
          ( ("greet", ([typeString], typeVoid)),
            \obj [mate] -> do
              lift $
                putStrLn $
                  "Hello, " <> (fromJust (fromDynamic mate) :: String) <> "!"
              return valueVoid
          )
        ]

    you = toDyn ("you" :: String)

    typeString = typeRep (Proxy @String)
    typeVoid = typeOf valueVoid
    valueVoid = toDyn ()
