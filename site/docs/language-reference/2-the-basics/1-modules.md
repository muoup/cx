# Modules

The first, and most apparent feature the language exposes are modules. You may have noticed the 'import' statements of the example code snippets shown previously. While standard `#include` headers are supported for backward compatibility reasons, import statements should be preferred in just about every case they can be used. The Chloro standard library, different from the C standard library which is also available, is accessed as modules via import statements.

```chl
import std::vector as std;
```