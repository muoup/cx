global_scope := 'typedef'? type_definition identifier? ';' | type_definition expression ';'
type_definition := base_type_def ('*' | '\[' number? '\]')*
base_type_def := struct_definition | enum_definition | union_definition | identifier
type_name := type_definition | intrinsic_type*

struct_definition := 'struct' identifier? '{' struct_body '}'
struct_body := type_definition identifier ';' struct_body?

enum_definition := 'enum' identifier? '{' enum_body '}'
enum_body := identifier ('=' number)? ',' enum_body?

union_definition := 'union' identifier? '{' union_body '}'
union_body := type_definition identifier ';' union_body?