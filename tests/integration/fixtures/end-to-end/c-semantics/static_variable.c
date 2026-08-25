/* CX-STDOUT: 0 */
/* CX-STDOUT-NEXT: 1 */
/* CX-STDOUT-NEXT: 2 */
/* CX-STDOUT-NEXT: 3 */
/* CX-STDOUT-NEXT: 4 */
/* CX-STDOUT-NEXT: 5 */
/* CX-STDOUT-NEXT: 6 */
/* CX-STDOUT-NEXT: 7 */
/* CX-STDOUT-NEXT: 8 */
/* CX-STDOUT-NEXT: 9 */

#include <stdio.h>

int counter() {
    static int i = 0;

    return i++;
}

int main() {
    for (int i = 0; i < 10; i++) {
        printf("%d\n", counter());
    }
}