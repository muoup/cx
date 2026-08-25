/* CX-STDOUT: */

int main(void) {
    int count = 0;
loop:
    count = count + 1;
    if (count < 3) {
        goto loop;
    }
    return count == 3 ? 0 : 1;
}
