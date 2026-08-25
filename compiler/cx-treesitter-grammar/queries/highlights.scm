(comment) @comment
(preprocessor_directive) @keyword
(string_literal) @string
(char_literal) @string
(number_literal) @number
(builtin_type) @type.builtin
(keyword) @keyword
(compiler_identifier) @keyword
(operator) @operator

(type (qualified_name) @type)
(type (templated_name) @type)
(struct_definition name: (identifier) @type)
(union_definition name: (identifier) @type)
(enum_definition name: (identifier) @type)
(enum_union_definition name: (identifier) @type)
(typedef_declarator (identifier) @type)
(template_parameter (identifier) @type.parameter)
(function_definition declarator: (function_declarator (callable_name) @function))
(function_declaration (function_declarator (callable_name) @function))
(comptime_function_definition name: (callable_name) @function)
(parameter (declarator (identifier) @variable.parameter))
(comptime_parameter (identifier) @variable.parameter)
(field_declarator (declarator (identifier) @property))
