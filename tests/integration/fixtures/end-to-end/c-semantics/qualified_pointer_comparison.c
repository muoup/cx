/* CX-STDOUT: 1 1 0 0 */
/* CX-STDOUT-NEXT: 1 1 1 */
/* CX-STDOUT-NEXT: 1 2 1 */

int printf(const char* fmt, ...);

struct holder { int* p; };

int main() {
    int values[2] = { 1, 2 };
    int* mutable_ptr = &values[0];
    const int* const_ptr = &values[0];
    const int* other = &values[1];
    volatile int* volatile_ptr = &values[0];
    void* untyped = &values[0];
    struct holder h = { &values[0] };

    printf("%d %d %d %d\n", mutable_ptr == const_ptr, const_ptr == mutable_ptr,
           mutable_ptr == other, other < mutable_ptr);
    printf("%d %d %d\n", h.p == const_ptr, const_ptr == volatile_ptr, untyped == const_ptr);

    const int* left = values[0] ? mutable_ptr : other;
    const int* right = values[0] ? other : mutable_ptr;
    const void* fallback = values[0] ? (void*)0 : const_ptr;
    printf("%d %d %d\n", *left, *right, fallback == (void*)0);
    return 0;
}
