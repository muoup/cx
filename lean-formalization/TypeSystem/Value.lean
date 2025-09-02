import TypeSystem.Expression

inductive Value : Expr -> Prop
    | unit              : Value Expr.unit
    | int { n : Int }   : Value (Expr.int_literal n)

def isValue : Expr -> Bool
    | Expr.unit => true
    | Expr.int_literal .. => true

    | Expr.add .. => false
