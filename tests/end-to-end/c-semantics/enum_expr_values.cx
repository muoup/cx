#include <stdio.h>

#define _ISbit(bit) ((bit) < 8 ? ((1 << (bit)) << 8) : ((1 << (bit)) >> 8))

enum {
    _ISupper = _ISbit(0),
    _ISlower = _ISbit(1),
    _ISnext
};

int arr[_ISupper == 256 ? 1 : -1];

int main() {
    printf("%d %d %d\n", _ISupper, _ISlower, _ISnext);
}
