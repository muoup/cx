/**
 * @file Chloro grammar for tree-sitter
 * @license MIT
 */

/// <reference types="tree-sitter-cli/dsl" />
// @ts-nocheck

const KEYWORDS = [
    "alignof",
    "as",
    "break",
    "case",
    "class",
    "comptime",
    "const",
    "continue",
    "default",
    "defer",
    "do",
    "else",
    "emit",
    "enum",
    "extern",
    "false",
    "fn",
    "for",
    "goto",
    "if",
    "import",
    "inline",
    "match",
    "move",
    "post",
    "pre",
    "private",
    "public",
    "register",
    "restrict",
    "return",
    "safe",
    "sizeof",
    "static",
    "strong",
    "struct",
    "switch",
    "template",
    "then",
    "thread_local",
    "true",
    "type",
    "typedef",
    "union",
    "volatile",
    "weak",
    "where",
    "while",
    "yield",
];

const BUILTIN_TYPES = [
    "auto",
    "bool",
    "char",
    "double",
    "float",
    "int",
    "long",
    "short",
    "signed",
    "unsigned",
    "void",
    "i8",
    "i16",
    "i32",
    "i64",
    "i128",
    "u8",
    "u16",
    "u32",
    "u64",
    "u128",
    "usize",
    "isize",
    "f32",
    "f64",
    "_Bool",
    "_char",
    "_Complex",
    "__complex",
    "__complex__",
    "__builtin_va_list",
    "_str",
    "unreachable",
];

const OPERATORS = [
    "::",
    "=>",
    "->",
    "<|",
    "|>",
    "==",
    "!=",
    "<=",
    ">=",
    "&&",
    "||",
    "<<",
    ">>",
    "++",
    "--",
    "...",
    "+=",
    "-=",
    "*=",
    "/=",
    "%=",
    "&=",
    "|=",
    "^=",
    "is",
    "+",
    "-",
    "*",
    "/",
    "%",
    "=",
    "<",
    ">",
    "!",
    "&",
    "|",
    "^",
    "~",
];

const PREC = {
    assignment: 1,
    conditional: 2,
    pipe: 3,
    logical_or: 4,
    logical_and: 5,
    bitwise_or: 6,
    bitwise_xor: 7,
    bitwise_and: 8,
    equality: 9,
    comparison: 10,
    shift: 11,
    additive: 12,
    multiplicative: 13,
    cast: 14,
    unary: 15,
    call: 16,
};

const commaSep1 = (rule) => seq(rule, repeat(seq(",", rule)), optional(","));
const keyword = ($, value) => alias(value, $.keyword);
const op = ($, value) => alias(value, $.operator);

const typeRule = ($, allowFunction, allowModifiers = true) =>
    seq(
        repeat($.type_qualifier),
        choice(
            $.builtin_type_sequence,
            keyword($, "type"),
            ...(allowFunction ? [$.function_type] : []),
            $.staged_type,
            $.strong_type,
            allowModifiers ? $.computed_type : prec.dynamic(1, $.computed_type),
            $.type_application,
            $.templated_name,
            $.qualified_name,
            $.struct_type,
            $.union_type,
            $.enum_type,
            $.enum_union_type,
        ),
        ...(allowModifiers ? [repeat(choice($._function_indirection, $.array_modifier))] : []),
    );

module.exports = grammar({
    name: "chl",

    extras: ($) => [
        /[\s\u00a0\u1680\u2000-\u200a\u2028\u2029\u202f\u205f\u3000]+/,
        $.comment,
    ],

    word: ($) => $.identifier,

    inline: ($) => [
        $._decl_type, $._decl_base_type, $._fn_decl_type, $._declared_type,
    ],

    reserved: {
        global: (_) => [],
        match: (_) => ["default"],
    },

    conflicts: ($) => [
        [$.declaration_specifier, $.access_section],
        [$.compiler_attribute, $.primary_expression],
        [$._top_level_item, $._unlabeled_statement],
        [$.type, $.primary_expression],
        [$.templated_name, $.type, $.primary_expression],
        [$.type],
        [$.type, $.type_application],
        [$.type, $.type_application, $.primary_expression],
        [$.decl_type, $.primary_expression],
        [$.decl_type, $.type_literal],
        [$.type, $.type_literal],
        [$.decl_type, $.type_application],
        [$.decl_type, $.decl_base_type],
        [$.decl_type, $.decl_base_type, $.primary_expression],
        [$.decl_type, $.decl_base_type, $.type_literal],
        [$.decl_base_type, $.primary_expression],
        [$.decl_base_type, $.type_literal],
        [$.decl_base_type, $.type_application],
        [$.decl_base_type, $.type_application, $.primary_expression],
        [$.type_application, $.primary_expression],
        [$.builtin_type, $.variable_declaration],
        [$.builtin_type, $.function_declaration],
        [$.builtin_type, $.variable_declaration, $.function_declaration],
        [$.struct_type, $.type_literal],
        [$.union_type, $.type_literal],
        [$.anonymous_struct_body, $.struct_definition],
        [$.anonymous_union_body, $.union_definition],
        [$.template_argument, $.literal],
        [$.template_argument, $.type, $.primary_expression],
        [$.staged_continuation_lambda, $.staged_lambda],
        [$.match_statement, $.match_expression],
        [$.compound_statement, $.initializer_list],
        [$.expression_statement, $.initializer_item],
        [$.primary_expression, $.compound_initializer],
        [$.defer_statement, $.primary_expression],
        [$.primary_expression, $.emit_expression],
        [$.expression, $.conditional_condition],
        [$.templated_name, $.primary_expression],
        [$.templated_name, $.type],
    ],

    rules: {
        source_file: ($) => repeat($._top_level_item),

        _top_level_item: ($) =>
            choice(
                $.preprocessor_directive,
                $.import_declaration,
                $.access_section,
                $.comptime_function_definition,
                $.type_definition,
                $.function_definition,
                $.overwrite_declaration,
                $.function_declaration,
                $.declaration,
                $._statement,
            ),

        keyword: (_) => choice(...KEYWORDS),

        builtin_type: (_) => choice(...BUILTIN_TYPES),

        builtin_type_sequence: ($) => repeat1($.builtin_type),

        operator: (_) => token(choice(...OPERATORS)),

        punctuation: (_) => token(choice(";", ",", ":", ".", "?")),

        identifier: (_) => /[A-Za-z_][A-Za-z0-9_]*/,

        compiler_identifier: (_) => /@[A-Za-z_][A-Za-z0-9_]*(\.[A-Za-z_][A-Za-z0-9_]*)*/,

        compiler_attribute: ($) =>
            prec.right(seq($.compiler_identifier, optional($.parenthesized_group))),

        qualified_name: ($) =>
            prec.right(
                seq(
                    $.identifier,
                    repeat(seq(op($, "::"), $.identifier)),
                ),
            ),

        templated_name: ($) => seq($.qualified_name, $.template_arguments),

        callable_name: ($) => choice($.templated_name, $.qualified_name),

        template_parameters: ($) =>
            seq("<", commaSep1($.template_parameter), ">"),

        template_parameter: ($) =>
            seq($.identifier, optional(seq(":", $.type))),

        template_arguments: ($) =>
            seq("<", commaSep1($.template_argument), ">"),

        template_argument: ($) =>
            choice($.type, $.number_literal, $.qualified_name, $.expression),

        type: ($) => typeRule($, true),

        decl_type: ($) => typeRule($, false),

        decl_base_type: ($) => typeRule($, false, false),

        _decl_base_type: ($) => alias($.decl_base_type, $.type),

        fn_decl_type: ($) => seq(repeat($.type_qualifier), $.function_type),

        _decl_type: ($) => alias($.decl_type, $.type),

        _fn_decl_type: ($) => alias($.fn_decl_type, $.type),

        _declared_type: ($) => choice($._decl_type, $._fn_decl_type),

        function_type: ($) =>
            seq(
                keyword($, "fn"),
                repeat($._function_indirection),
                $.parameter_list,
                op($, "->"),
                $.type,
            ),

        function_type_declarator: ($) =>
            seq(
                keyword($, "fn"),
                repeat($._function_indirection),
                field("name", $.identifier),
                repeat($.array_modifier),
                $.parameter_list,
                op($, "->"),
                $.type,
            ),

        _function_indirection: ($) => choice($.pointer_modifier, $.reference_modifier),

        strong_type: ($) => prec.right(seq(keyword($, "@strong"), $.type)),

        computed_type: ($) =>
            seq(keyword($, "@type"), "(", choice($.expression, prec.dynamic(-1, $.type)), ")"),

        staged_type: ($) => prec.right(seq(op($, "^"), $.type)),

        type_application: ($) =>
            seq($.qualified_name, "(", commaSep1(choice($.type, $.expression)), ")"),

        type_qualifier: ($) =>
            choice(
                keyword($, "const"),
                keyword($, "restrict"),
                keyword($, "__restrict"),
                keyword($, "volatile"),
                keyword($, "register"),
            ),

        pointer_modifier: ($) =>
            seq(
                op($, "*"),
                optional($.type_qualifier),
            ),

        reference_modifier: ($) =>
            seq(optional($.lifetime_modifier), op($, "&")),

        lifetime_modifier: ($) =>
            seq("'", choice($.identifier, keyword($, "static"))),

        array_modifier: ($) => seq("[", optional($.expression), "]"),

        struct_type: ($) =>
            prec.right(
                seq(
                    keyword($, "struct"),
                    optional(
                        choice(
                            $.templated_name,
                            $.qualified_name,
                        ),
                    ),
                    optional($.type_attributes),
                    optional($.anonymous_struct_body),
                ),
            ),

        union_type: ($) =>
            prec.right(
                seq(
                    keyword($, "union"),
                    optional(
                        choice(
                            $.templated_name,
                            $.qualified_name,
                        ),
                    ),
                    optional($.type_attributes),
                    optional($.anonymous_union_body),
                ),
            ),

        enum_type: ($) =>
            prec.right(
                seq(
                    keyword($, "enum"),
                    optional($.qualified_name),
                ),
            ),

        enum_union_type: ($) =>
            seq(
                keyword($, "enum"),
                keyword($, "union"),
                optional($.type_attributes),
                "{",
                optional(commaSep1($.union_variant)),
                "}",
            ),

        anonymous_struct_body: ($) =>
            seq("{", repeat($.field_declaration), "}"),

        anonymous_union_body: ($) =>
            seq("{", repeat($.field_declaration), "}"),

        type_attributes: ($) => prec.right(seq(":", commaSep1($.compiler_attribute))),

        type_definition: ($) =>
            choice(
                $.struct_definition,
                $.union_definition,
                $.enum_definition,
                $.enum_union_definition,
                $.typedef_definition,
            ),

        struct_definition: ($) =>
            seq(
                keyword($, "struct"),
                optional(field("name", $.identifier)),
                optional($.template_parameters),
                optional($.type_attributes),
                choice(
                    seq(
                        "{",
                        repeat($.field_declaration),
                        "}",
                        ";",
                    ),
                    ";",
                ),
            ),

        union_definition: ($) =>
            seq(
                keyword($, "union"),
                optional(field("name", $.identifier)),
                optional($.template_parameters),
                optional($.type_attributes),
                choice(
                    seq(
                        "{",
                        repeat($.field_declaration),
                        "}",
                        ";",
                    ),
                    ";",
                ),
            ),

        enum_definition: ($) =>
            seq(
                keyword($, "enum"),
                optional(field("name", $.identifier)),
                optional(seq(":", field("underlying", $.enum_underlying_type))),
                "{",
                repeat(choice($.preprocessor_directive, seq($.enum_member, ","))),
                optional(seq($.enum_member, repeat($.preprocessor_directive))),
                "}",
                ";",
            ),

        enum_underlying_type: ($) =>
            seq(optional(keyword($, "@strong")), alias("int", $.builtin_type)),

        enum_union_definition: ($) =>
            seq(
                keyword($, "enum"),
                keyword($, "union"),
                optional(field("name", $.identifier)),
                optional($.type_attributes),
                "{",
                optional(commaSep1($.union_variant)),
                "}",
                ";",
            ),

        typedef_definition: ($) =>
            seq(
                keyword($, "typedef"),
                optional($.template_parameters),
                $.type,
                commaSep1($.typedef_declarator_with_initializer),
                ";",
            ),

        typedef_declarator_with_initializer: ($) =>
            seq(
                $.typedef_declarator,
                optional(seq(op($, "="), $.expression)),
            ),

        typedef_declarator: ($) =>
            choice(
                $.function_pointer_declarator,
                seq(
                    optional($.compiler_attribute),
                    $.identifier,
                    optional($.parameter_list),
                    repeat($.array_modifier),
                ),
            ),

        field_declaration: ($) =>
            seq(
                choice(
                    seq($._decl_base_type, commaSep1($.field_declarator)),
                    $.function_type_declarator,
                ),
                ";",
            ),

        field_declarator: ($) =>
            seq(
                $.declarator,
                optional(seq(":", $.expression)),
            ),

        enum_member: ($) =>
            seq(
                $.identifier,
                optional(seq(op($, "="), $.expression)),
            ),

        union_variant: ($) => seq($.identifier, op($, "::"), $.type),

        declaration_specifier: ($) =>
            choice(
                keyword($, "extern"),
                keyword($, "inline"),
                keyword($, "private"),
                keyword($, "public"),
                keyword($, "static"),
                keyword($, "strong"),
                keyword($, "thread_local"),
                keyword($, "weak"),
            ),

        declaration: ($) =>
            seq(
                optional(keyword($, "comptime")),
                repeat($.declaration_specifier),
                $.variable_declaration,
                ";",
            ),

        variable_declaration: ($) =>
            choice(
                seq($._decl_base_type, commaSep1($.declarator_with_initializer)),
                seq(
                    $.function_type_declarator,
                    optional(seq(op($, "="), $.expression)),
                ),
                seq(
                    repeat($.type_qualifier),
                    alias("auto", $.builtin_type),
                    $.identifier,
                    ":",
                    $.type,
                    optional(seq(op($, "="), $.expression)),
                ),
            ),

        declarator_with_initializer: ($) =>
            seq(
                $.declarator,
                optional(seq(op($, "="), $.expression)),
            ),

        declarator: ($) => seq(repeat($._function_indirection), $._direct_declarator),

        _direct_declarator: ($) =>
            choice(
                $.function_pointer_declarator,
                seq(
                    optional($.compiler_attribute),
                    $.identifier,
                    repeat($.array_modifier),
                ),
            ),

        function_pointer_declarator: ($) =>
            seq(
                "(",
                repeat($.pointer_modifier),
                $.identifier,
                ")",
                $.parameter_list,
                repeat($.array_modifier),
            ),

        function_declarator: ($) =>
            seq(
                $.callable_name,
                $.parameter_list,
            ),

        function_definition: ($) =>
            prec(
                2,
                seq(
                    repeat($.declaration_specifier),
                    choice(
                        seq(
                            field("return_type", $._declared_type),
                            field("declarator", $.function_declarator),
                            optional($.function_contract),
                        ),
                        seq(
                            repeat($.type_qualifier),
                            alias("auto", $.builtin_type),
                            field("declarator", $.function_declarator),
                            optional($.function_contract),
                            op($, "->"),
                            field("return_type", $.type),
                        ),
                    ),
                    $.function_body,
                ),
            ),

        function_declaration: ($) =>
            seq(
                repeat($.declaration_specifier),
                optional(keyword($, "comptime")),
                choice(
                    seq($._declared_type, $.function_declarator, optional($.function_contract)),
                    seq(
                        repeat($.type_qualifier),
                        alias("auto", $.builtin_type),
                        $.function_declarator,
                        optional($.function_contract),
                        op($, "->"),
                        $.type,
                    ),
                ),
                ";",
            ),

        overwrite_declaration: ($) =>
            seq(alias("@overwrite", $.compiler_identifier), choice($.declaration, $.function_declaration)),

        comptime_function_definition: ($) =>
            prec(
                3,
                seq(
                    keyword($, "comptime"),
                    choice(
                        seq(
                            field("return_type", $._declared_type),
                            field("name", $.callable_name),
                            field("parameters", $.parameter_list),
                            optional($.function_contract),
                        ),
                        seq(
                            repeat($.type_qualifier),
                            alias("auto", $.builtin_type),
                            field("name", $.callable_name),
                            field("parameters", $.parameter_list),
                            optional($.function_contract),
                            op($, "->"),
                            field("return_type", $.type),
                        ),
                    ),
                    choice($.function_body, $.transparent_type_body),
                ),
            ),

        transparent_type_body: ($) => seq(op($, "="), $.type, ";"),

        function_body: ($) => choice(
            $.compound_statement,
            seq(op($, "=>"), choice($.expression, $.type), ";"),
        ),

        parameter_list: ($) =>
            seq(
                "(",
                optional(
                    choice(
                        seq(commaSep1($.parameter), ",", op($, "...")),
                        commaSep1($.parameter),
                        op($, "..."),
                    ),
                ),
                ")",
            ),

        parameter: ($) =>
            choice(
                seq(
                    optional(keyword($, "comptime")),
                    choice(
                        seq($._decl_type, optional(alias($._direct_declarator, $.declarator))),
                        $._fn_decl_type,
                    ),
                    optional($.parameter_default),
                ),
                $.function_type_declarator,
            ),

        parameter_default: ($) => seq(op($, "="), $.expression),

        function_contract: ($) =>
            choice(
                seq(
                    keyword($, "safe"),
                    optional(
                        seq(
                            keyword($, "where"),
                            commaSep1($.contract_clause),
                        ),
                    ),
                ),
                seq(
                    keyword($, "where"),
                    commaSep1($.contract_clause),
                ),
            ),

        contract_clause: ($) =>
            seq(
                choice(
                    keyword($, "pre"),
                    seq(
                        keyword($, "post"),
                        optional(seq("(", $.identifier, ")")),
                    ),
                ),
                ":",
                "(",
                $.expression,
                ")",
            ),

        import_declaration: ($) =>
            seq(
                keyword($, "import"),
                $.import_path,
                optional(seq(keyword($, "as"), $.qualified_name)),
                ";",
            ),

        import_path: ($) =>
            seq(
                repeat(seq($.identifier, op($, "::"))),
                choice($.identifier, $.import_group),
            ),

        import_group: ($) => seq("{", commaSep1($.import_path), "}"),

        access_section: ($) =>
            seq(
                repeat1(
                    choice(
                        keyword($, "public"),
                        keyword($, "private"),
                        keyword($, "extern"),
                    ),
                ),
                optional($.string_literal),
                ":",
            ),

        _statement: ($) =>
            choice($._unlabeled_statement, $.labeled_statement),

        _block_item: ($) =>
            choice(
                $._unlabeled_statement,
                $.declaration,
                alias($._labeled_block_item, $.labeled_statement),
            ),

        _unlabeled_statement: ($) =>
            choice(
                $.compound_statement,
                $.if_statement,
                $.while_statement,
                $.do_statement,
                $.for_statement,
                $.switch_statement,
                $.match_statement,
                $.defer_statement,
                $.return_statement,
                $.yield_statement,
                $.break_statement,
                $.continue_statement,
                $.goto_statement,
                $.unpack_statement,
                $.compiler_statement,
                $.staged_continuation_statement,
                $.expression_statement,
                $.preprocessor_directive,
            ),

        compound_statement: ($) => seq("{", repeat($._block_item), "}"),

        if_statement: ($) =>
            prec.right(
                seq(
                    keyword($, "if"),
                    "(",
                    $.expression,
                    ")",
                    $._statement,
                    optional(seq(keyword($, "else"), $._statement)),
                ),
            ),

        while_statement: ($) =>
            seq(
                keyword($, "while"),
                "(",
                $.expression,
                ")",
                $._statement,
            ),

        do_statement: ($) =>
            seq(
                keyword($, "do"),
                $._statement,
                keyword($, "while"),
                "(",
                $.expression,
                ")",
                ";",
            ),

        for_statement: ($) =>
            seq(
                keyword($, "for"),
                "(",
                optional(choice($.variable_declaration, $.expression)),
                ";",
                optional($.expression),
                ";",
                optional($.expression),
                ")",
                $._statement,
            ),

        switch_statement: ($) =>
            seq(
                keyword($, "switch"),
                "(",
                $.expression,
                ")",
                "{",
                repeat($.switch_clause),
                "}",
            ),

        switch_clause: ($) =>
            seq(
                choice(
                    seq(keyword($, "case"), $.expression),
                    keyword($, "default"),
                ),
                ":",
                repeat($._block_item),
            ),

        match_statement: ($) =>
            seq(
                keyword($, "match"),
                "(",
                $.expression,
                ")",
                $._match_body,
            ),

        _match_body: ($) =>
            seq("{", optional(commaSep1($.match_arm)), "}"),

        match_arm: ($) =>
            seq(
                $.match_pattern,
                op($, "=>"),
                choice(
                    $.expression,
                    alias($._arm_return, $.return_statement),
                    alias($._arm_yield, $.yield_statement),
                ),
            ),

        _arm_return: ($) =>
            prec.right(seq(keyword($, "return"), optional($.expression))),

        _arm_yield: ($) =>
            prec.right(seq(keyword($, "yield"), optional($.expression))),

        match_pattern: ($) =>
            choice(
                $.number_literal,
                seq(op($, "-"), $.number_literal),
                $.binding_pattern,
                $._pattern_path,
                $.variant_pattern,
            ),

        binding_pattern: ($) =>
            seq(
                optional(keyword($, "const")),
                alias("auto", $.builtin_type),
                optional(op($, "&")),
                $.identifier,
            ),

        _pattern_path: ($) =>
            choice(
                reserved("match", $.identifier),
                alias($._pattern_scoped_name, $.qualified_name),
            ),

        _pattern_scoped_name: ($) =>
            prec(
                1,
                seq(
                    reserved("match", $.identifier),
                    repeat1(seq(op($, "::"), $.identifier)),
                ),
            ),

        variant_pattern: ($) =>
            prec(
                1,
                seq(
                    $._pattern_path,
                    "(",
                    optional($.match_pattern),
                    ")",
                ),
            ),

        defer_statement: ($) =>
            seq(
                keyword($, "defer"),
                choice($.staged_block, $.expression),
                ";",
            ),

        return_statement: ($) =>
            seq(keyword($, "return"), optional($.expression), ";"),

        yield_statement: ($) =>
            seq(keyword($, "yield"), optional($.expression), ";"),

        break_statement: ($) => seq(keyword($, "break"), ";"),

        continue_statement: ($) => seq(keyword($, "continue"), ";"),

        goto_statement: ($) =>
            seq(
                keyword($, "goto"),
                choice($.identifier, seq(op($, "*"), $.expression)),
                ";",
            ),

        compiler_statement: ($) =>
            seq(
                $.compiler_attribute,
                choice(
                    $.compound_statement,
                    $.expression_statement,
                ),
            ),

        unpack_statement: ($) =>
            seq(
                alias("@unpack", $.compiler_identifier),
                "(",
                $.expression,
                ")",
                "{",
                optional(commaSep1($.unpack_binding)),
                "}",
                ";",
            ),

        unpack_binding: ($) => seq($.identifier, ":", $.identifier),

        labeled_statement: ($) =>
            prec.right(seq($.identifier, ":", optional($._statement))),

        _labeled_block_item: ($) =>
            prec.right(seq($.identifier, ":", optional($._block_item))),

        staged_continuation_statement: ($) => $.staged_continuation_expression,

        staged_continuation_expression: ($) =>
            prec.left(
                PREC.pipe,
                seq(
                    $.expression,
                    choice(op($, "<|"), op($, "|>")),
                    $.staged_continuation_lambda,
                ),
            ),

        staged_continuation_lambda: ($) =>
            seq(
                $._lambda_parameters,
                choice(
                    $.staged_then,
                    seq(op($, "=>"), $.staged_continuation_expression),
                ),
            ),

        expression_statement: ($) => seq(optional(seq($.expression, repeat(seq(",", $.expression)))), ";"),

        expression: ($) =>
            choice(
                $.assignment_expression,
                $.conditional_expression,
                $.binary_expression,
                $.as_expression,
                $.unary_expression,
                $.postfix_expression,
                $.primary_expression,
            ),

        assignment_expression: ($) =>
            prec.right(
                PREC.assignment,
                seq(
                    choice(
                        $.unary_expression,
                        $.as_expression,
                        $.postfix_expression,
                        $.primary_expression,
                    ),
                    choice(
                        op($, "="),
                        op($, "+="),
                        op($, "-="),
                        op($, "*="),
                        op($, "/="),
                        op($, "%="),
                        op($, "&="),
                        op($, "|="),
                        op($, "^="),
                    ),
                    $.expression,
                ),
            ),

        conditional_expression: ($) =>
            prec.right(
                PREC.conditional,
                seq(
                    $.conditional_condition,
                    "?",
                    $.expression,
                    repeat(seq(",", $.expression)),
                    ":",
                    $.expression,
                ),
            ),

        conditional_condition: ($) =>
            choice(
                $.binary_expression,
                $.as_expression,
                $.unary_expression,
                $.postfix_expression,
                $.primary_expression,
            ),

        binary_expression: ($) =>
            choice(
                prec.left(PREC.pipe, seq($.expression, choice(op($, "<|"), op($, "|>"), $.indexed_pipe), $.expression)),
                prec.left(PREC.logical_or, seq($.expression, op($, "||"), $.expression)),
                prec.left(PREC.logical_and, seq($.expression, op($, "&&"), $.expression)),
                prec.left(PREC.bitwise_or, seq($.expression, op($, "|"), $.expression)),
                prec.left(PREC.bitwise_xor, seq($.expression, op($, "^"), $.expression)),
                prec.left(PREC.bitwise_and, seq($.expression, op($, "&"), $.expression)),
                prec.left(PREC.equality, seq($.expression, choice(op($, "=="), op($, "!=")), $.expression)),
                prec.left(PREC.comparison, seq($.expression, choice(op($, "<"), op($, ">"), op($, "<="), op($, ">=")), $.expression)),
                prec.left(PREC.shift, seq($.expression, choice(op($, "<<"), op($, ">>")), $.expression)),
                prec.left(PREC.additive, seq($.expression, choice(op($, "+"), op($, "-")), $.expression)),
                prec.left(PREC.multiplicative, seq($.expression, choice(op($, "*"), op($, "/"), op($, "%")), $.expression)),
            ),

        as_expression: ($) =>
            prec.left(PREC.cast, seq(
                field("value", $.expression),
                keyword($, "as"),
                field("target", choice($.expression, prec.dynamic(-1, $.type))),
            )),

        unary_expression: ($) =>
            prec(
                PREC.unary,
                seq(
                    choice(
                        op($, "!"),
                        op($, "~"),
                        op($, "+"),
                        op($, "-"),
                        op($, "&"),
                        op($, "*"),
                        op($, "++"),
                        op($, "--"),
                        keyword($, "move"),
                    ),
                    $.expression,
                ),
            ),

        indexed_pipe: (_) => token(prec(1, seq("|>(", /\s*\d+\s*/, ")"))),

        postfix_expression: ($) =>
            prec.left(
                PREC.call,
                seq(
                    $.primary_expression,
                    repeat1(
                        choice(
                            $.argument_list,
                            seq(choice(".", op($, "->"), op($, "::")), $.identifier),
                            seq("[", $.expression, "]"),
                            choice(op($, "++"), op($, "--"), op($, "!")),
                            seq(op($, "is"), $.match_pattern),
                        ),
                    ),
                ),
            ),

        argument_list: ($) =>
            seq("(", optional(commaSep1(choice($.named_argument, $.expression, prec.dynamic(-1, $.type)))), ")"),

        named_argument: ($) =>
            seq(".", field("name", $.identifier), op($, "="), field("value", choice($.expression, prec.dynamic(-1, $.type)))),

        primary_expression: ($) =>
            choice(
                $.literal,
                $.templated_name,
                $.qualified_name,
                $.compiler_identifier,
                $.type_literal,
                $.cast_expression,
                $.parenthesized_expression,
                $.unit_literal,
                $.compound_initializer,
                $.initializer_list,
                $.staged_block,
                $.staged_lambda,
                $.match_expression,
                $.sizeof_expression,
                $.emit_expression,
                $.then_expression,
                $.label_address_expression,
            ),

        type_literal: ($) => choice(
            $.strong_type,
            $.computed_type,
            seq(keyword($, "struct"), $.anonymous_struct_body),
            seq(keyword($, "union"), $.anonymous_union_body),
        ),

        label_address_expression: ($) => seq(op($, "&&"), $.identifier),

        literal: ($) =>
            choice(
                $.number_literal,
                $.string_literal,
                $.char_literal,
                keyword($, "true"),
                keyword($, "false"),
            ),

        unit_literal: (_) => seq("(", ")"),

        parenthesized_expression: ($) => seq("(", $.expression, repeat(seq(",", $.expression)), ")"),

        cast_expression: ($) => seq("(", $.type, ")", $.expression),

        compound_initializer: ($) => seq("(", $.type, ")", $.initializer_list),

        initializer_list: ($) =>
            seq(
                "{",
                optional(commaSep1($.initializer_item)),
                "}",
            ),

        initializer_item: ($) =>
            seq(
                optional(seq(".", $.identifier, op($, "="))),
                $.expression,
            ),

        staged_block: ($) => seq(".", $.compound_statement),

        staged_lambda: ($) =>
            seq(
                $._lambda_parameters,
                choice(
                    $.staged_then,
                    seq(
                        op($, "=>"),
                        choice(
                            $.expression,
                            alias($._arm_return, $.return_statement),
                            alias($._arm_yield, $.yield_statement),
                        ),
                    ),
                ),
            ),

        _lambda_parameters: ($) =>
            seq("|", optional(commaSep1(choice(seq($._decl_type, $.identifier), $.function_type_declarator))), "|"),

        staged_then: ($) => prec(20, keyword($, "then")),

        match_expression: ($) =>
            seq(
                keyword($, "match"),
                "(",
                $.expression,
                ")",
                $._match_body,
            ),

        sizeof_expression: ($) =>
            seq(
                choice(keyword($, "sizeof"), keyword($, "alignof")),
                "(",
                choice($.type, $.expression),
                ")",
            ),

        emit_expression: ($) =>
            seq(
                keyword($, "emit"),
                choice(
                    $.staged_block,
                    $.expression,
                    alias($._arm_return, $.return_statement),
                    alias($._arm_yield, $.yield_statement),
                ),
            ),

        then_expression: ($) => $.staged_then,

        preprocessor_directive: (_) => token(seq("#", /[^\n]*/)),

        number_literal: (_) =>
            token(
                choice(
                    /0[xX][0-9a-fA-F]+([uUlL]+)?/,
                    /0[bB][01]+([uUlL]+)?/,
                    /\d+\.\d+([eE][+-]?\d+)?[fFlL]?/,
                    /\d+[eE][+-]?\d+[fFlL]?/,
                    /\d+[uUlL]*/,
                ),
            ),

        string_literal: (_) =>
            token(seq('"', repeat(choice(/[^"\\\n]+/, /\\./)), '"')),

        char_literal: (_) => token(seq("'", choice(/[^'\\\n]/, /\\./), "'")),

        parenthesized_group: ($) => seq("(", repeat($._raw_element), ")"),

        _raw_element: ($) =>
            choice(
                $.parenthesized_group,
                $.bracket_group,
                $.compound_statement,
                $.preprocessor_directive,
                $.string_literal,
                $.char_literal,
                $.number_literal,
                $.compiler_identifier,
                $.keyword,
                $.builtin_type,
                $.identifier,
                $.operator,
                $.punctuation,
            ),

        bracket_group: ($) => seq("[", repeat($._raw_element), "]"),

        comment: (_) =>
            token(
                choice(
                    seq("//", /[^\n]*/),
                    seq("/*", /[^*]*\*+([^/*][^*]*\*+)*/, "/"),
                ),
            ),
    },
});
