module Greet where

import Control.Monad.State.Strict
import Data.Dynamic
import Data.Map.Strict
import Data.Maybe
import Data.Proxy
import Data.Typeable
import OOP
import Prelude hiding (lookup)

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
