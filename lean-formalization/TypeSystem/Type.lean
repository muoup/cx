-- Defines what a 'Ty' is standard C semantics, CX-extensions TBD

import Lean

inductive IntSize where
    | i8  : IntSize
    | i16 : IntSize
    | i32 : IntSize
    | i64 : IntSize
deriving Repr, DecidableEq

inductive FloatSize where
    | f32 : FloatSize
    | f64 : FloatSize
deriving Repr, DecidableEq

inductive Ty where
    | unit  : Ty
    | int   : (signed : Bool) → (size : IntSize) → Ty
    | float : (size : FloatSize) -> Ty
    | ptr   : (pointee : Ty) -> Ty
deriving Repr, DecidableEq

def IntSize.toNat : IntSize → Nat
    | IntSize.i8 => 1
    | IntSize.i16 => 2
    | IntSize.i32 => 4
    | IntSize.i64 => 8

def Ty.add (t1 t2 : Ty) : Option Ty :=
    match t1, t2 with
    | Ty.int signed1 size1, Ty.int signed2 size2 =>
        let signed := signed1 || signed2
        let size := if size1.toNat > size2.toNat then size1 else size2
        some (Ty.int signed size)
    | _, _ => none

example : Ty.add (Ty.int false IntSize.i32) (Ty.int true IntSize.i64) = some (Ty.int true IntSize.i64)
    := by rfl

example : Ty.add (Ty.int false IntSize.i16) (Ty.int false IntSize.i8) = some (Ty.int false IntSize.i16)
    := by rfl
