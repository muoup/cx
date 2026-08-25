/* CX-STDOUT: */

int values[] = { 1, 2, 3 };
int *alias = values;

int main(void) {
    return alias[1] == 2 ? 0 : 1;
}
