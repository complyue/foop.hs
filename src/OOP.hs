module OOP where

import Control.Monad.State.Strict
import Data.Dynamic
import Data.Map.Strict
import Data.Maybe
import Data.Typeable
import Type.Reflection (SomeTypeRep (..))
import Prelude hiding (lookup)

dynTypeOf :: Dynamic -> TypeRep
dynTypeOf (Dynamic tr _) = SomeTypeRep tr

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
type OOM = State ObjectWorld

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
