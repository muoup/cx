import Lean

import TypeSystem.Expression
import TypeSystem.Value
import TypeSystem.Type

inductive step : (e1 e2 : Expr) -> Prop where
    | stepl { e1 e1' e2 } :
        step e1 e1' ->
        step (Expr.add e1 e2) (Expr.add e1' e2)
    | stepr { e1 e2 e2' } :
        Value e1 ->
        step e2 e2' ->
        step (Expr.add e1 e2) (Expr.add e1 e2')
    | add { v1 v2 : Int } :
        step
            (Expr.add (Expr.int_literal v1) (Expr.int_literal v2))
            (Expr.int_literal (v1 + v2))
