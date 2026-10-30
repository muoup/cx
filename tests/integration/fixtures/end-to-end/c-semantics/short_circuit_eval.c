#include <stdio.h>

int getCondition1() {
    printf("Condition 1 evaluated\n");
    return 1;
}

int main() {
    int condition1 = 1;
    int condition2 = 0;

    printf("condition1 && getCondition1() = %d\n", condition1 && getCondition1());
    printf("condition2 && getCondition1() = %d\n", condition2 && getCondition1());

    printf("condition1 || getCondition1() = %d\n", condition1 || getCondition1());
    printf("condition2 || getCondition1() = %d\n", condition2 || getCondition1());
}
