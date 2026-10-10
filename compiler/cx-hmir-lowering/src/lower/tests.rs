use std::collections::HashMap;

use cx_hmir::{
    HMIRBinaryOp, HMIRBody, HMIRCoerceMode, HMIRContract, HMIRControlOp, HMIRDef, HMIRDefID,
    HMIRDefKind, HMIRFnSignature, HMIRFunction, HMIRFunctionStage, HMIROp, HMIRUnit,
    body::{HMIRLocal, HMIRLocalID},
    expr::{HMIRExpr, HMIRExprID, HMIRExprKind, constant::HMIRConstant},
    ty::{
        HMIRType, HMIRTypeID, HMIRTypeKind,
        aggregate::HMIRMoveSemantics,
        intrinsic::{HMIRIntType, HMIRIntWidth},
    },
    unit::function::{HMIRFnDefinition, HMIRFnParam},
};
use cx_mir::MIRUnit;
use cx_namespace::module::{NamespacePath, QualifiedName};
use cx_pipeline_data::db::ModuleData;
use cx_target::ArchitectureConfig;
use cx_tokens::TokenRange;
use cx_util::{identifier::CXIdent, linkage::LinkageMode};

use crate::{env::HMIREnvironment, lower::lower_unit};

struct Types {
    void: HMIRTypeID,
    bool: HMIRTypeID,
    i32: HMIRTypeID,
    u32: HMIRTypeID,
    i64: HMIRTypeID,
    i8: HMIRTypeID,
    ptr_i32: HMIRTypeID,
}

struct TestUnit {
    types: HashMap<HMIRTypeID, HMIRType>,
    defs: HashMap<HMIRDefID, HMIRDef>,
    scalar: Types,
}

struct TestFn {
    body: HMIRBody,
    params: Vec<(HMIRLocalID, HMIRFnParam)>,
}

impl TestUnit {
    fn new() -> Self {
        let mut types = HashMap::new();
        let mut add = |kind: HMIRTypeKind| {
            let id = HMIRTypeID::new(types.len());
            let ty = HMIRType::new(
                id,
                None,
                kind,
                HMIRMoveSemantics::POD,
                false,
                TokenRange::internal(),
            );
            types.insert(id, ty);
            id
        };

        let int = |width, signed| HMIRTypeKind::Int(HMIRIntType::new(width, signed));
        let scalar = Types {
            void: add(HMIRTypeKind::Void),
            bool: add(int(HMIRIntWidth::I1, false)),
            i32: add(int(HMIRIntWidth::I32, true)),
            u32: add(int(HMIRIntWidth::I32, false)),
            i64: add(int(HMIRIntWidth::I64, true)),
            i8: add(int(HMIRIntWidth::I8, true)),
            ptr_i32: HMIRTypeID::new(0),
        };
        let scalar = Types {
            ptr_i32: add(HMIRTypeKind::PointerTo(scalar.i32)),
            ..scalar
        };

        Self {
            types,
            defs: HashMap::new(),
            scalar,
        }
    }

    fn define(&mut self, name: &str, function: TestFn, return_ty: HMIRTypeID, root: HMIRExprID) {
        let mut function = function;
        let return_type = function.ty(return_ty);
        let locals = function.params.iter().map(|(local, _)| *local).collect();
        let params = function.params.into_iter().map(|(_, param)| param).collect();

        let signature = HMIRFnSignature::new(
            HMIRFunctionStage::Runtime,
            params,
            return_type,
            false,
            HMIRContract::default(),
        );
        let hmir = HMIRFunction::new(
            CXIdent::new(name),
            LinkageMode::Standard,
            function.body,
            signature,
            Some(HMIRFnDefinition::new(locals, root)),
        );

        let id = HMIRDefID::new(self.defs.len());
        let def = HMIRDef::new(
            QualifiedName::new_raw(CXIdent::new(name)),
            TokenRange::internal(),
            HMIRDefKind::Function(Box::new(hmir)),
        );
        self.defs.insert(id, def);
    }

    fn finish(self) -> HMIRUnit {
        HMIRUnit::new(NamespacePath::root(), self.types, self.defs)
    }
}

impl TestFn {
    fn new() -> Self {
        Self {
            body: HMIRBody::new(),
            params: Vec::new(),
        }
    }

    fn expr(&mut self, kind: HMIRExprKind) -> HMIRExprID {
        self.body
            .push_expr(HMIRExpr::new(kind, TokenRange::internal()))
    }

    fn ty(&mut self, ty: HMIRTypeID) -> HMIRExprID {
        self.expr(HMIRExprKind::Constant(HMIRConstant::Type(ty)))
    }

    fn param(&mut self, name: &str, ty: HMIRTypeID) -> HMIRExprID {
        let ty_expr = self.ty(ty);
        let local = self.body.declare_local(HMIRLocal::new(
            Some(CXIdent::new(name)),
            ty_expr,
            false,
            TokenRange::internal(),
        ));
        self.params.push((local, HMIRFnParam::new(false, ty_expr)));

        self.expr(HMIRExprKind::Local(local))
    }

    fn local(&mut self, name: &str, ty: HMIRTypeID) -> HMIRLocalID {
        let ty_expr = self.ty(ty);
        self.body.declare_local(HMIRLocal::new(
            Some(CXIdent::new(name)),
            ty_expr,
            false,
            TokenRange::internal(),
        ))
    }

    fn read(&mut self, local: HMIRLocalID) -> HMIRExprID {
        self.expr(HMIRExprKind::Local(local))
    }

    fn int(&mut self, value: i128, ty: HMIRTypeID) -> HMIRExprID {
        self.expr(HMIRExprKind::Constant(HMIRConstant::Int { value, ty }))
    }

    fn binary(&mut self, op: HMIRBinaryOp, lhs: HMIRExprID, rhs: HMIRExprID) -> HMIRExprID {
        self.expr(HMIRExprKind::Op(HMIROp::BinOp { op, lhs, rhs }))
    }

    fn coerce(&mut self, value: HMIRExprID, target: HMIRTypeID) -> HMIRExprID {
        let target = self.ty(target);
        self.expr(HMIRExprKind::Op(HMIROp::Coerce {
            mode: HMIRCoerceMode::Implicit,
            value,
            target,
        }))
    }

    fn ret(&mut self, value: Option<HMIRExprID>) -> HMIRExprID {
        self.expr(HMIRExprKind::Op(HMIROp::Control(HMIRControlOp::Return(value))))
    }

    fn block(&mut self, statements: Vec<HMIRExprID>) -> HMIRExprID {
        self.expr(HMIRExprKind::Block { statements })
    }

    fn declare(&mut self, local: HMIRLocalID, initializer: Option<HMIRExprID>) -> HMIRExprID {
        self.expr(HMIRExprKind::Let { local, initializer })
    }
}

fn lower(unit: HMIRUnit) -> MIRUnit {
    let global = ModuleData::new();
    let mut env = HMIREnvironment::new(&global, &unit, ArchitectureConfig::native());

    if let Err(error) = lower_unit(&unit, &mut env) {
        panic!("lowering failed: {} {}", error.code(), error.message());
    }

    env.finish()
}

fn lower_printed(unit: HMIRUnit) -> (MIRUnit, String) {
    let mir = lower(unit);
    let printed = mir.display_pretty().to_string();
    (mir, printed)
}

fn binary_function(op: HMIRBinaryOp, operand: fn(&Types) -> HMIRTypeID, result: fn(&Types) -> HMIRTypeID) -> String {
    let mut unit = TestUnit::new();
    let (operand, result) = (operand(&unit.scalar), result(&unit.scalar));

    let mut function = TestFn::new();
    let a = function.param("a", operand);
    let b = function.param("b", operand);
    let value = function.binary(op, a, b);
    let ret = function.ret(Some(value));
    let root = function.block(vec![ret]);
    unit.define("op", function, result, root);

    lower_printed(unit.finish()).1
}

#[test]
fn sum_of_two_parameters() {
    let mut unit = TestUnit::new();
    let i32 = unit.scalar.i32;

    let mut function = TestFn::new();
    let a = function.param("a", i32);
    let b = function.param("b", i32);
    let sum = function.binary(HMIRBinaryOp::Add, a, b);
    let ret = function.ret(Some(sum));
    let root = function.block(vec![ret]);
    unit.define("add", function, i32, root);

    let (mir, printed) = lower_printed(unit.finish());

    let (_, function) = mir.functions().next().expect("function was declared");
    assert_eq!(function.prototype().symbol_name.as_str(), "add");
    let body = function.body().expect("function was defined");
    assert_eq!(body.parameters().count(), 2);

    assert!(printed.contains("int.add"), "{printed}");
    assert!(printed.contains("return"), "{printed}");
    assert!(printed.contains("initialize"), "{printed}");
}

#[test]
fn signed_and_unsigned_division_select_different_instructions() {
    let signed = binary_function(HMIRBinaryOp::Div, |t| t.i32, |t| t.i32);
    assert!(signed.contains("int.s_div"), "{signed}");
    assert!(!signed.contains("int.u_div"), "{signed}");

    let unsigned = binary_function(HMIRBinaryOp::Div, |t| t.u32, |t| t.u32);
    assert!(unsigned.contains("int.u_div"), "{unsigned}");
    assert!(!unsigned.contains("int.s_div"), "{unsigned}");
}

#[test]
fn signed_and_unsigned_comparison_select_different_instructions() {
    let signed = binary_function(HMIRBinaryOp::Lt, |t| t.i32, |t| t.bool);
    assert!(signed.contains("int.s_lt"), "{signed}");
    assert!(!signed.contains("int.u_lt"), "{signed}");

    let unsigned = binary_function(HMIRBinaryOp::Lt, |t| t.u32, |t| t.bool);
    assert!(unsigned.contains("int.u_lt"), "{unsigned}");
    assert!(!unsigned.contains("int.s_lt"), "{unsigned}");
}

#[test]
fn mixed_signedness_operands_become_unsigned() {
    let mut unit = TestUnit::new();
    let (i32, u32) = (unit.scalar.i32, unit.scalar.u32);

    let mut function = TestFn::new();
    let a = function.param("a", i32);
    let b = function.param("b", u32);
    let quotient = function.binary(HMIRBinaryOp::Div, a, b);
    let ret = function.ret(Some(quotient));
    let root = function.block(vec![ret]);
    unit.define("mixed", function, u32, root);

    let printed = lower_printed(unit.finish()).1;
    assert!(printed.contains("int.u_div"), "{printed}");
}

#[test]
fn let_with_implicitly_converted_initializer() {
    let mut unit = TestUnit::new();
    let (i32, i64) = (unit.scalar.i32, unit.scalar.i64);

    let mut function = TestFn::new();
    let a = function.param("a", i32);
    let wide = function.local("wide", i64);
    let init = function.coerce(a, i64);
    let declaration = function.declare(wide, Some(init));
    let read = function.read(wide);
    let ret = function.ret(Some(read));
    let root = function.block(vec![declaration, ret]);
    unit.define("widen", function, i64, root);

    let (_, printed) = lower_printed(unit.finish());
    assert!(printed.contains("int.int_cast"), "{printed}");
    assert!(printed.contains("true"), "sign extension expected: {printed}");
    assert!(printed.contains("%wide"), "{printed}");
}

#[test]
fn let_converts_initializer_to_declared_type() {
    let mut unit = TestUnit::new();
    let (i32, i64) = (unit.scalar.i32, unit.scalar.i64);

    let mut function = TestFn::new();
    let a = function.param("a", i32);
    let wide = function.local("wide", i64);
    let declaration = function.declare(wide, Some(a));
    let read = function.read(wide);
    let ret = function.ret(Some(read));
    let root = function.block(vec![declaration, ret]);
    unit.define("widen", function, i64, root);

    let (_, printed) = lower_printed(unit.finish());
    assert!(printed.contains("int.int_cast"), "{printed}");
}

#[test]
fn if_else_returning_from_both_branches() {
    let mut unit = TestUnit::new();
    let i32 = unit.scalar.i32;

    let mut function = TestFn::new();
    let a = function.param("a", i32);
    let b = function.param("b", i32);
    let condition = function.binary(HMIRBinaryOp::Lt, a, b);
    let a = function.expr(HMIRExprKind::Local(function.params[0].0));
    let b = function.expr(HMIRExprKind::Local(function.params[1].0));
    let then_return = function.ret(Some(a));
    let else_return = function.ret(Some(b));
    let branch = function.expr(HMIRExprKind::If {
        condition,
        then_branch: then_return,
        else_branch: Some(else_return),
    });
    let root = function.block(vec![branch]);
    unit.define("min", function, i32, root);

    let (mir, printed) = lower_printed(unit.finish());

    assert!(printed.contains("int.s_lt"), "{printed}");
    assert!(printed.contains("if "), "{printed}");
    assert_eq!(printed.matches("return").count(), 2, "{printed}");
    assert!(!printed.contains("if.merge"), "{printed}");

    let (_, function) = mir.functions().next().unwrap();
    let body = function.body().unwrap();
    for (_, block) in body.blocks() {
        let last = block.last_instruction().expect("block has instructions");
        assert!(last.is_terminator(), "{printed}");
    }
}

#[test]
fn if_else_merges_values() {
    let mut unit = TestUnit::new();
    let i32 = unit.scalar.i32;

    let mut function = TestFn::new();
    let a = function.param("a", i32);
    let b = function.param("b", i32);
    let condition = function.binary(HMIRBinaryOp::Lt, a, b);
    let a = function.expr(HMIRExprKind::Local(function.params[0].0));
    let b = function.expr(HMIRExprKind::Local(function.params[1].0));
    let branch = function.expr(HMIRExprKind::If {
        condition,
        then_branch: a,
        else_branch: Some(b),
    });
    let ret = function.ret(Some(branch));
    let root = function.block(vec![ret]);
    unit.define("min", function, i32, root);

    let (_, printed) = lower_printed(unit.finish());
    assert!(printed.contains("if.merge"), "{printed}");
    assert_eq!(printed.matches("return").count(), 1, "{printed}");
}

#[test]
fn while_loop() {
    let mut unit = TestUnit::new();
    let (i32, void) = (unit.scalar.i32, unit.scalar.void);

    let mut function = TestFn::new();
    let n = function.param("n", i32);
    let counter = function.local("i", i32);
    let zero = function.int(0, i32);
    let declaration = function.declare(counter, Some(zero));

    let current = function.read(counter);
    let condition = function.binary(HMIRBinaryOp::Lt, current, n);
    let step = function.read(counter);
    let increment = function.expr(HMIRExprKind::Op(HMIROp::UnOp {
        op: cx_hmir::HMIRUnaryOp::PostIncrement,
        operand: step,
    }));
    let loop_body = function.block(vec![increment]);
    let looped = function.expr(HMIRExprKind::While {
        condition,
        body: loop_body,
        pre_eval: true,
    });

    let root = function.block(vec![declaration, looped]);
    unit.define("count", function, void, root);

    let (mir, printed) = lower_printed(unit.finish());

    assert!(printed.contains("while.condition"), "{printed}");
    assert!(printed.contains("while.body"), "{printed}");
    assert!(printed.contains("while.exit"), "{printed}");
    assert!(printed.contains("int.s_lt"), "{printed}");
    assert!(printed.contains("int.add"), "{printed}");

    let (_, function) = mir.functions().next().unwrap();
    let body = function.body().unwrap();
    for (_, block) in body.blocks() {
        assert!(block.last_instruction().unwrap().is_terminator(), "{printed}");
    }
}

#[test]
fn do_while_enters_the_body_first() {
    let mut unit = TestUnit::new();
    let (i32, void) = (unit.scalar.i32, unit.scalar.void);

    let mut function = TestFn::new();
    let n = function.param("n", i32);
    let condition = function.binary(HMIRBinaryOp::Lt, n, n);
    let empty = function.block(Vec::new());
    let looped = function.expr(HMIRExprKind::While {
        condition,
        body: empty,
        pre_eval: false,
    });
    let root = function.block(vec![looped]);
    unit.define("once", function, void, root);

    let (_, printed) = lower_printed(unit.finish());
    let entry = printed.find("bb0").expect("entry block");
    let jump = printed[entry..].find("goto bb2").expect("jump into the body");
    let condition = printed[entry..].find("bb1 /* while.condition");
    assert!(condition.is_some_and(|position| jump < position), "{printed}");
}

#[test]
fn logical_and_short_circuits() {
    let mut unit = TestUnit::new();
    let (i32, bool) = (unit.scalar.i32, unit.scalar.bool);

    let mut function = TestFn::new();
    let a = function.param("a", i32);
    let b = function.param("b", i32);
    let both = function.binary(HMIRBinaryOp::LAnd, a, b);
    let ret = function.ret(Some(both));
    let root = function.block(vec![ret]);
    unit.define("both", function, bool, root);

    let (_, printed) = lower_printed(unit.finish());
    assert!(printed.contains("logical.rhs"), "{printed}");
    assert!(printed.contains("logical.merge"), "{printed}");
    assert_eq!(printed.matches("int.neq").count(), 2, "{printed}");
}

#[test]
fn pointer_arithmetic_scales_by_element_size() {
    let mut unit = TestUnit::new();
    let (i32, ptr) = (unit.scalar.i32, unit.scalar.ptr_i32);

    let mut function = TestFn::new();
    let pointer = function.param("p", ptr);
    let index = function.param("i", i32);
    let moved = function.binary(HMIRBinaryOp::Add, pointer, index);
    let ret = function.ret(Some(moved));
    let root = function.block(vec![ret]);
    unit.define("offset", function, ptr, root);

    let (_, printed) = lower_printed(unit.finish());
    assert!(printed.contains("pointer.add"), "{printed}");
    assert!(printed.contains("int.s_mul"), "{printed}");
    assert!(printed.contains("4:I64"), "{printed}");
}

#[test]
fn pointer_comparison_uses_pointer_intrinsics() {
    let mut unit = TestUnit::new();
    let (ptr, bool) = (unit.scalar.ptr_i32, unit.scalar.bool);

    let mut function = TestFn::new();
    let lhs = function.param("a", ptr);
    let rhs = function.param("b", ptr);
    let less = function.binary(HMIRBinaryOp::Lt, lhs, rhs);
    let ret = function.ret(Some(less));
    let root = function.block(vec![ret]);
    unit.define("before", function, bool, root);

    let (_, printed) = lower_printed(unit.finish());
    assert!(printed.contains("pointer.lt"), "{printed}");
}

#[test]
fn narrow_integers_are_promoted_with_sign_extension() {
    let mut unit = TestUnit::new();
    let (i32, i8) = (unit.scalar.i32, unit.scalar.i8);

    let mut function = TestFn::new();
    let a = function.param("a", i8);
    let b = function.param("b", i8);
    let sum = function.binary(HMIRBinaryOp::Add, a, b);
    let ret = function.ret(Some(sum));
    let root = function.block(vec![ret]);
    unit.define("narrow", function, i32, root);

    let (_, printed) = lower_printed(unit.finish());
    assert_eq!(printed.matches("int.int_cast").count(), 2, "{printed}");
    assert!(printed.contains("I32, true"), "{printed}");
}

#[test]
fn void_function_falls_through_to_return() {
    let mut unit = TestUnit::new();
    let (i32, void) = (unit.scalar.i32, unit.scalar.void);

    let mut function = TestFn::new();
    let a = function.param("a", i32);
    let ignored = function.binary(HMIRBinaryOp::Add, a, a);
    let root = function.block(vec![ignored]);
    unit.define("ignore", function, void, root);

    let (_, printed) = lower_printed(unit.finish());
    assert!(printed.trim_end().ends_with("return;\n}"), "{printed}");
}
