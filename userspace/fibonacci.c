#include "lib.h"

static unsigned exchange(unsigned* x, unsigned y) {
    unsigned res = *x;
    *x = y;
    return res;
}

int main() {
    unsigned i = 0, j = 1;
    for (;;) {
        i = exchange(&j, i + j);
        printf("%u ", i);
    }
    return 0;
}
