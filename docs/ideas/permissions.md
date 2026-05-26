```cpp

// In 'safe' contexts, we restrict language features such that we can safely reason about the capacity of its semantics
// trusted contexts handle FFI with potentially unsafe code and define explicit permissions required to invoke different
// procedures, in untrusted contexts, each routine is granted a set of permissions defining what side effects it may have.
// 
// The goal with this syntax is for supply-chain safety. If a smaller package is compromised, its infected code only has
// the power of the permissions it is granted. While it would be convenient, it is important that untrusted files are
// unable to define their own compound permissions for easier syntax as granting some routine `foo::permissions` allows
// the foo package to redefine that permission set at-will to grant itself elevated permissions.
void create_config_file(...)
    safe<std::fileops>
{
    ...
}

```

The permission system is to be handled in the typechecker with simple semantics. Each function defines its own required permissions to be granted. As defined by the project configuration, modules in a project are all either "trusted" or "untrusted", an untrusted function may not use `@unsafe` blocks. In a trusted module, all safe functions are allowed to use `@unsafe` blocks at their discretion. If function f with permission p_f wants to invoke function g with permissions p_g, p_g must be a subset of p_f, otherwise this function call is unsafe and must be wrapped explicitly by an `@unsafe` marker if in a safe context.

Permissions defined on a function should be treated as a normal property of functions and should be permitted to be variable over templated arguments. They are handled at compile time, and thus are sound to be passed as templated arguments itself. A function like `vector<T>::map` for instance would need templated permissions for privileged manipulation of the inner data.

The platonic project shape this would encourage is one where only the main project and the CX standard library are marked as trusted, in contexts with tighter security, one may even strive to have just the CX standard library marked as trusted, with explicit permission handling throughout the project itself. This of course will not always be possible, some necessary C code is simply uninsurable with this system, like inline assembly and interfacing with libc. It would be anticipated then that libraries wrapping unsafe functionality like an ffmpeg wrapper would require explicitly given trust and define its own granual permission set to use its interfaces in safe contexts.

While this plan seems promising, there are some unknowns here that need better fleshing out.
    - Function pointers: is it the action of creating a function pointer which requires privilege, or invoking it?
    - Implicit Permissions: if passing permissions are implicit -- i.e. when foo with permissions f_p calls bar with permissions g_p, the expression `bar()` automatically infers the permission to be passed, we seemingly open ourselves up to attack, see below:

```cpp

ulong complete_request(request* request) safe<std::clock> { ... };

void foo(...) safe<std::clock, std::fileops> {
    vector<request> requests = ...;
    ulong average_completion_time = iter::from(requests)
        .map(complete_request)
        .sum() / requests.length;

    printf("Fulfilling requests took an average of %.2f seconds\n", (float) average_completion_time / 1000);
}

// Safe definition of an iterator map function, uses permissions of the provided anonymous function as expected
void iterator<T: Type, U: Type, P: Permission>::map(*self, U(*fn)(T) safe<P>)
    safe<P> { ... }

// Without needing to change the foo function signature, suddenly this function gives the compromised code uncontrolled
// access to the user's filesystem. We are trying to avoid this with the 'safe' system, and there is good likelihood that
// in cases like this, all uses of iterator::map in a project will not happen to be in a functions that happen to also have
// filesystem access, but this justification is flaky.
void iterator<T: Type, U: Type, P: Permission>::map(*self, U(*fn)(T) safe<P>)
    safe<P + std::fileops> { ... }

```