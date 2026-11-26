# Types

## Primitive Types

On top of C's base intrinsic types like `int` and `float`, Chloro provides a set of fixed-size types for predictable
behavior across different architectures and operating systems:

- `i8`, `i16`, `i32`, `i64`, `i128`: n-bit signed integers
- `u8`, `u16`, `u32`, `u64`, `u128`: n-bit unsigned integers
- `f32`, `f64`: n-bit floating-point values
- `usize`, `isize`: pointer-sized signed/unsigned integer types

## Strong Types

To ensure that special integer types are used only as they are intended, one may use the `@strong` tag to dictate that the type tagged is *not* directly equivalent to one of the same inner form. This may be applied to any type, both simple integral types, as well as structs, enums, etc. 

The `@strong` indicator does not accept any identifier, guaranteeing only a unique identity for a provided type on evaluation. As such, two `@strong int` types would not be equivalent as they produce separate identities per evaluation.

If one is certain that their `@strong` type can be safely coerced to another, a strong type can be coerced to either its underlying or another strong type of equivalent underlying type via an explicit C-style cast.

```chl title="units"

typedef @strong int celcius;
typedef @strong int fahrenheit;

celcius f_to_c(fahrenheit temp) => (temp - 32) * 5 / 9;
celcius c_to_f(celsius temp) => (temp * 9/5) + 32;

int main() {
    celcius measurement = 20;

    // Whoops, we tried to pass a Celcius measurement as a Fahrenheit type, this errors
    fahrenheit converted = c_to_f(measurement);
    // This works, but be careful, that doesn't mean its correct!
    fahrenheit converted = c_to_f((celcius) measurement);
}

```

### Strong Enums

An additional use-case for strong types comes with enumerations. When defining an enum type, if the enum is defined to have a `@strong` underlying type using standard C10 syntax, it becomes a special-case "strong enum", ensuring not only that the values of the enum are of said uniquely-identified type, but also that the generated enum constants are scoped under the enum namespace.

For now, do also note that the underlying type of an enum is only supported as either an `int` or a `@strong int`.

```chl
enum socket_type : @strong int {
    STREAM = 1,
    DGRAM,
};

socket_type kind = socket_type::DGRAM;
// DGRAM is an unknown symbol
socket_type kind = DGRAM;
```