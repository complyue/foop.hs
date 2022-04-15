# Functional OOP

> Object Oriented Programming implemented in Functional Paradigm

- [Enabler (Haskell, 90 LoC)](#enabler-haskell-90-loc)
- [Counter Example](#counter-example)
  - [Haskell (FOOP, 30 LoC)](#haskell-foop-30-loc)
  - [C++ (10 LoC)](#c-10-loc)
  - [Python (10 LoC)](#python-10-loc)
- [Greet Example](#greet-example)
  - [Haskell (FOOP, 70 LoC)](#haskell-foop-70-loc)
  - [Python (20 LoC)](#python-20-loc)

## Enabler (Haskell, 90 LoC)

[OOP.hs](./src/OOP.hs)

<details>

<summary>
Expand to see 90+ lines
</summary>

```haskell
type ObjectIdent = Int

nextOid :: ObjectIdent -> ObjectIdent
nextOid = (+ 1)

type PropertyIdent = String

type MethodIdent = (PropertyIdent, MethodSignature)

type MethodSignature = ([TypeRep], TypeRep)

type MethodImplementation = Object -> [Dynamic] -> OOM Dynamic

type ObjectState = Map PropertyIdent Dynamic

data Class = Class
  { class_name :: PropertyIdent,
    super_class :: Maybe Class,
    obj_constructors :: Map [TypeRep] ([Dynamic] -> OOM ObjectState),
    instance_methods :: Map MethodIdent MethodImplementation
    -- static methods omitted here, that's actually simpler than instance methods
  }

data Object = Object
  { oid :: ObjectIdent,
    obj_class :: Class,
    obj_internals :: ObjectState
  }

data ObjectWorld = ObjectWorld
  { next_oid :: ObjectIdent,
    classes :: Map PropertyIdent Class,
    population :: Map ObjectIdent Object
  }

{- Object Oriented Monad
note: "Out Of Memory" never expected -}
type OOM = StateT ObjectWorld IO

defineClass ::
  PropertyIdent ->
  Maybe PropertyIdent ->
  Map [TypeRep] ([Dynamic] -> OOM ObjectState) ->
  Map MethodIdent MethodImplementation ->
  OOM Class
defineClass name superName ctors mths = do
  world <- get
  let cls =
        Class
          { class_name = name,
            super_class = fromJust . flip lookup (classes world) <$> superName,
            obj_constructors = ctors,
            instance_methods = mths
          }
  put $ world {classes = insert name cls (classes world)}
  return cls

newObject :: Class -> [Dynamic] -> OOM ObjectIdent
newObject cls ctorArgs = do
  world <- get
  let ctor = lookup (dynTypeOf <$> ctorArgs) (obj_constructors cls)
  internals <- fromJust ctor ctorArgs
  let oid = next_oid world
      obj =
        Object
          { oid = oid,
            obj_class = cls,
            obj_internals = internals
          }
  put
    world
      { next_oid = nextOid (next_oid world),
        population = insert oid obj (population world)
      }
  return oid

deref :: ObjectIdent -> OOM Object
deref oid = fromJust . lookup oid . population <$> get

invokeMethod :: ObjectIdent -> PropertyIdent -> [Dynamic] -> TypeRep -> OOM Dynamic
invokeMethod oid name args expectReturn = do
  obj <- deref oid
  let mth = resolveMethod (Just $ obj_class obj)
  fromJust mth obj args
  where
    sig = (dynTypeOf <$> args, expectReturn)
    resolveMethod :: Maybe Class -> Maybe MethodImplementation
    resolveMethod Nothing = Nothing
    resolveMethod (Just cls) = case lookup (name, sig) (instance_methods cls) of
      Nothing -> resolveMethod (super_class cls)
      Just mth -> return mth
```

</details>

## Counter Example

### Haskell (FOOP, 30 LoC)

[Counter.hs](./src/Counter.hs)

<details>

<summary>
Expand to see 30+ lines
</summary>

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

</details>

### C++ (10 LoC)

[counter.cxx](./oop/counter.cxx)

<details>

<summary>
Expand to see 10+ lines
</summary>

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

</details>

### Python (10 LoC)

[counter.py](./oop/counter.py)

<details>

<summary>
Expand to see 10+ lines
</summary>

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

</details>

## Greet Example

### Haskell (FOOP, 70 LoC)

[Greet.hs](./src/Greet.hs)

<details>

<summary>
Expand to see 70+ lines
</summary>

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

</details>

### Python (20 LoC)

[greet.py](./oop/greet.py)

<details>

<summary>
Expand to see 20+ lines
</summary>

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

</details>
