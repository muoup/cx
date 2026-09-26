/* CX-STDOUT: 92 39 34 */

#include <stdio.h>
#define CHARACTER_ADDRESS &'a'

int main() {
    char slash = '\\';
    char quote = '\'';
    char double_quote = '"';

    printf("%d %d %d\n", slash, quote, double_quote);
}
