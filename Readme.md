# Functional OOP

> Object Oriented Programming implemented in Functional Paradigm

- [Counter](#counter)
  - [Haskell (FOOP)](#haskell-foop)
  - [C++](#c)
  - [Python](#python)
- [Greet](#greet)
  - [Haskell (FOOP)](#haskell-foop-1)
  - [Python](#python-1)

## Counter

### Haskell (FOOP)

[Counter.hs](./src/Counter.hs)

```haskell
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
```

### C++

[counter.cxx](./oop/counter.cxx)

```c++
class Counter {
private:
  int n;

public:
  Counter(int ctor_n) : n(ctor_n) {}
  int increase() { return ++this->n; }
};

int main() {
  Counter *counter = new Counter(7);
  counter->increase();
  int result = counter->increase();
  cout << "Done, result= " << result << endl;
  delete counter;
}

```

### Python

[counter.py](./oop/counter.py)

```py
class Counter:
    def __init__(self, n: int) -> None:
        self.n = n

    def increase(self) -> int:
        self.n += 1
        return self.n


counter = Counter(7)

counter.increase()

result = counter.increase()

print("Done, result= ", result)
```

## Greet

### Haskell (FOOP)

[Greet.hs](./src/Greet.hs)

```haskell
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
```

### Python

[greet.py](./oop/greet.py)

```py
class Animal:
    def greet(self, mate):
        print(f"{type(self).__name__} remains silent facing {mate}.")

class Spider(Animal):
    pass

class Cat(Animal):
    def greet(self, mate):
        print(f"Meow, meow!")

class Dog(Animal):
    def greet(self, mate):
        print(f"Woof, woof!")

class People(Animal):
    def greet(self, mate):
        print(f"Hello {mate}!")

def main():
    Spider().greet("you")
    Cat().greet("you")
    Dog().greet("you")
    People().greet("you")

main()
```
