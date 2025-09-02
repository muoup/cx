import TypeSystem.Semantics.Operations
import TypeSystem.Value

theorem progress { e t } { ht : has_type e t } : (Value e) ∨ (∃ e', step e e') := by
    induction ht with
    | unit =>
        left; exact Value.unit
    | int_literal =>
        left; apply Value.int;
    | add ht1 ht2 a progress_e1 progress_e2 =>
        right;

        cases progress_e1 with
        | inl v1 =>
            cases progress_e2 with
            | inl v2 =>
                cases v1; cases v2;
                exists Expr.int_literal (a + b);
                apply step.add;
            | inr step_e2 =>
                exists (Expr.add e1 e2');
                apply step.stepr; assumption; assumption;
        | inr step_e1 => right; exact step.add step_e1
