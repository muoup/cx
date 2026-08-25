/* CX-STDOUT: nested lhs and = 0 */
/* CX-STDOUT-NEXT: nested lhs or = 1 */
/* CX-STDOUT-NEXT: nested rhs and = 1 */
/* CX-STDOUT-NEXT: nested rhs or = 1 */
/* CX-STDOUT-NEXT: mixed nesting = 1 */

#include <stdio.h>

int main() {
    int true_value = 1;
    int false_value = 0;

    printf("nested lhs and = %d\n", true_value && true_value && false_value);
    printf("nested lhs or = %d\n", false_value || false_value || true_value);
    printf("nested rhs and = %d\n", true_value && (false_value || true_value));
    printf("nested rhs or = %d\n", false_value || (true_value && true_value));
    printf("mixed nesting = %d\n",
           false_value || (true_value && true_value &&
                           (false_value || true_value)));
}
