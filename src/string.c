#include "string.h"

usize strlen(const char* s) {
    if (*s == 0) {
        return 0;
    }
    return 1 + strlen(s + 1);
}

int strcmp(const char* a, const char* b) {
    if (*a == 0 && *b == 0) {
        return 0;
    }
    if (*a < *b) {
        return -1;
    }
    if (*a > *b) {
        return 1;
    }
    return strcmp(a + 1, b + 1);
}
