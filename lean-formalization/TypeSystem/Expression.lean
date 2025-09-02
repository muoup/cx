import Lean
import TypeSystem.Type

inductive BinaryOperation where
    | add
    | sub
deriving Repr, DecidableEq

inductive Expr where
    | unit              : Expr
    | int_literal       : Int -> Expr
    | add               : Expr -> Expr -> Expr

inductive has_type : Expr -> Ty -> Prop
    | unit        : has_type Expr.unit Ty.unit
    | int_literal : has_type (Expr.int_literal _) (Ty.int _ _)
    | add { e1 e2 t1 t2 t } :
        has_type e1 t1 ->
        has_type e2 t2 ->

        Ty.add t1 t2 = some t ->
        has_type (Expr.add e1 e2) t
