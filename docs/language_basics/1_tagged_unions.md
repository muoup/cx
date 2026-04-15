# Tagged Unions


CX supports a first-class form of the 'tagged union' pattern, also known as a sum type, discriminated union, variant, etc. The syntax for declaring such a type is as follows:


```c++
enum union TaggedUnion {
    union_variant_1 :: UnionType1,
    union_variant_2 :: UnionType2,
    union_variant_3 :: UnionType3
    // ...
};
```


Where:

- `union_variant_[N]` is an identifier
- `UnionType[N]` is a valid type, including identifiers mapped with a 'typedef' statement and in-place type definitions


For example, a simplified implementation of a JSON object may look as follows:

```c++
enum union json {
    object :: vec<json_object_index>,
    array  :: vec<json>,    // note that objects are stored in-place, so recursive indices are only valid with an indirection layer.
    number :: f64,
    string :: string,
    bool   :: bool,
    null   :: void          // void here indicates a zero-byte type, making `Null` just a tag with no inner content. 
};

struct json_object_index {
    string name;
    json object;
};
```

When accessing an enum union, you have generally two different options, an `is` statement, or a `match` statement.

## The `Is` Operator

```c++

opt<string> get_string(json* json_object) {
    if (*json_object is json::string(str)) {
        return opt<string>::some(str.clone());
    }
    
    return opt<string>::none();
}

```

The `is` operator is a binary operation taking in a tagged union to its left, and a pattern to its right. The `is` operator itself returns a boolean and unconditionally maps a symbol, if provided, to a reference to the inner variant. Note that because this is unconditional, it is not always valid to access said symbol, if the result of the `is` statement is false, then accessing the symbol is undefined behavior. For example:

```c++
void invalid_usage(json* json_object) {
    // No matter what, 'str' is a valid symbol after evaluation regardless of the result of the 'is' operator and conditionally gating 
    // its usage is not required, if you know for certain that json_object is a certain index due to an unformalized invariant,
    // you opt to use an assertion / assumption instead of a conditional gate:  
    *json_object is json::string(str);
    
    // For instance:
    assert(*json_object is json::string(str)); 
    printf("string contents: ", str.c_str());
}
```

# The `match` Operator

The match operator is a general-purpose alternative to C's switch statement. The main difference between a `switch` statement
and a `match` statement is that the latter requires all cases to map to an inner-scope, and as such fall-throughs instead break
from the case rather than falling through to the next. Tagged unions may only be used in `match` statements due to their need
for proper symbol scoping, and may be used as follows:

```c++

void print_json(json_object* obj) {
    match (*obj) {
        json::string(str)   => printf("str: %s", str.c_str());
        json::number(num)   => printf("num: %f", num);
        json::bool(b)       => printf("bool: %d", b);
        json::null()        => printf("null");
        
        json::array(arr) => {
            printf("[");
            
            for (usize i = 0; i < arr.size(); i++) {
                print_json(arr.at(i));
            }
            
            printf("]");
        }
        
        json::object(indices) => {
            printf("{");
            
            for (usize i = 0; i < arr.size(); i++) {
                json_object_index* index = arr.at(i);
                
                printf("\"%s\": ", index.name.c_str());
                print_json(&index.object);
            }
            
            printf("}");
        }
    }
}

```