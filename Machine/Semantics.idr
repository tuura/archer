module Machine.Semantics

import Data.Fin
import Machine.Types
import Machine.Instruction

%default total
%access public export
%hide Const

data Key : Type where
  Reg  : Register -> Key
  Addr : MemoryAddress -> Key
  F    : Flag -> Key
  IC   : Key

ValueType : Key -> Type
ValueType (Reg x) = Value
ValueType (Addr x) = Value
ValueType (F x) = Bool
ValueType (IC)  = Byte

-- Semantics : (c : (Type -> Type) -> Type) ->
--             (a : Type) -> Type
-- Semantics c a = {f : (Type -> Type)} ->
--      (c f => (k : Key) -> f (ValueType k)) ->
--      (c f => (k : Key) -> f (ValueType k) -> f ()) ->
--      (c f => Maybe (f a))

-- using (f : Type -> Type)
--     Semantics : (c : (Type -> Type) -> Type) ->
--                 (a : Type) -> Type
--     Semantics c a =
--         (c f => ((k : Key) -> f (ValueType k)) ->
--                 ((k : Key) -> f (ValueType k) -> f ()) ->
--                 (Maybe (f a)))

Semantics : (c : (Type -> Type) -> Type) ->
            (a : Type) -> Type
Semantics c a =
    {f : Type -> Type} ->
    (c f => ((k : Key) -> f (ValueType k)) ->
            ((k : Key) -> f (ValueType k) -> f ()) ->
            (Maybe (f a)))

load : (r : Register) -> (addr : MemoryAddress) ->
       Semantics Functor ()
load reg addr read write = Just $
  write (Reg reg) (read (Addr addr))

jump : Byte -> Semantics Functor ()
jump offset read write = Just $
    write IC ((+ offset) <$> (read IC))

add : (r : Register) -> (addr : MemoryAddress) ->
      Semantics Applicative ()
add r addr read write = Just $
  let result = (+) <$> read (Reg r) <*> read (Addr addr)
      isZero = (== 0) <$> result
  in  write (Reg r) result *> write (F Zero) isZero

-- add' : {g : Type -> Type} -> (r : Register) -> (addr : MemoryAddress) ->
--        Semantics {f=g} Applicative ()
-- add' r addr read write = Just $
--   let result = (+) <$> read (Reg r) <*> read (Addr addr)
--       isZero = (== 0) <$> result
--   in  write (Reg r) result *> write (F Zero) isZero



semanticsF : Instruction -> Semantics Functor ()
semanticsF (Load r addr) = load r addr
semanticsF (Jump offset) = jump offset
semanticsF i             = const (const Nothing)

semanticsA : Instruction -> Semantics Applicative ()
semanticsA (Add r addr) = add r addr
semanticsA i            = semanticsF i

blockSemanticsA : List Instruction -> Semantics Applicative ()
blockSemanticsA xs read write =
    foldr (\x,acc => ((*>)) <$> acc <*> semanticsA x read write) nop xs
    where nop = Just $ pure ()

--------------------------------------------------------------------------------

data Const a b = MkConst a

Functor (Const a) where
    map _ (MkConst x) = MkConst x

Monoid a => Applicative (Const a) where
    pure x = MkConst neutral
    (MkConst x) <*> (MkConst y) = MkConst (x <+> y)

getConst : Const a b -> a
getConst (MkConst c) = c

-- dependencies : Semantics Applicative a -> Maybe (List Key, List Key)
-- dependencies task =
--     partitionEithers . getConst <$>
--     task {f=Const (List (Either Key Key))} trackingRead trackingWrite
--   where trackingRead  k    = MkConst (Left k :: Nil)
--         trackingWrite k fv = fv *> MkConst (Right k :: Nil)

-- trackingRead : (k : Key) -> Const (List (Either Key Key)) () -- (ValueType k)
trackingRead : (k : Key) -> Const (List (Either Key Key)) (ValueType k)
trackingRead  k    = MkConst (Left k :: Nil)

trackingWrite : (k : Key) -> Const (List (Either Key Key)) (ValueType k) ->
                Const (List (Either Key Key)) () -- (ValueType k)
trackingWrite k fv = fv *> MkConst (Right k :: Nil)


-- -- dependencies : Semantics Applicative a -> Maybe (Const (List (Either Key Key)) a)
-- dependencies : Semantics Applicative a -> Maybe (List Key, List Key)
-- dependencies task =
--     partitionEithers . getConst <$>
--     task trackingRead trackingWrite
-- --   where trackingRead  k    = MkConst (Left k :: Nil)
-- --         trackingWrite k fv = fv *> MkConst (Right k :: Nil)

||| Currently, due to bad type inference, only this variant of the 'dependencies'
||| function works.
dependencies' : -- Semantics Applicative a ->
                Maybe (List Key, List Key)
dependencies' = -- task =
    partitionEithers . getConst <$>
    (blockSemanticsA block) trackingRead trackingWrite
    where block = [Load R0 0
                  , Add R0 1
                  ]

-- dependencies'' : Semantics Applicative () ->
--                 Maybe (Const (List (Either Key Key)) ())
-- dependencies'' task =
--     task trackingRead trackingWrite

-- s : Semantics Functor ()
-- s = load R0 0

-- s' : Maybe (Const (List (Either Key Key)) ())
-- s' = s trackingRead trackingWrite *> pure ()

-- {f=Const (List (Either Key Key))}

-- deps : Maybe (List Key, List Key)
-- deps = dependencies (\r,w => s r w)

-- IdFunc : {a : Type} -> Type
-- IdFunc =

someNat : ({a : Type} -> a -> a) -> Nat
someNat id' = id' 3

Task : (c : (Type -> Type) -> Type) ->
       (a : Type) -> Type
Task c a = {f : Type -> Type} -> (c f => f a)

f : Task Applicative ()
f = pure ()

f' : Const (List ()) ()
f' = f

someNatF : ({a : Type} -> a -> a) -> Nat
someNatF id' = id' 3