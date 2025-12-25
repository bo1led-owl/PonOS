#include "lib.h"

#include "types.h"

static void printString(const char* s) {
    for (const char* c = s; *c; ++c) {
        printChar(*c);
    }
}

static void printUnsigned(u32 n, u8 radix) {
    if (n == 0) {
        printChar('0');
        return;
    }

    // worst case is binary, when we put a char for each of 32 bits
    char buf[33];

    usize i = 0;
    while (n > 0) {
        u32 digit = n % radix;
        char formattedDigit = (digit < 10) ? '0' + digit : 'a' + (digit - 10);
        buf[i] = formattedDigit;
        i += 1;
        n /= radix;
    }

    buf[i] = 0;

    for (usize j = 0; j < i / 2; ++j) {
        char tmp = buf[j];
        buf[j] = buf[i - j - 1];
        buf[i - j - 1] = tmp;
    }

    printString(buf);
}

static void printSigned(i32 n, u8 radix) {
    if (n < 0) {
        printChar('-');
        n *= -1;
    }

    printUnsigned(n, radix);
}

void vprintf(const char* fmt, va_list args) {
    bool seenPercent = false;
    for (const char* c = fmt; *c; ++c) {
        if (seenPercent) {
            switch (*c) {
                case 'd': {
                    i32 n = va_arg(args, i32);
                    printSigned(n, 10);
                    break;
                }
                case 'u': {
                    u32 n = va_arg(args, u32);
                    printUnsigned(n, 10);
                    break;
                }
                case 'b': {
                    u32 n = va_arg(args, u32);
                    printUnsigned(n, 2);
                    break;
                }
                case 'x': {
                    u32 n = va_arg(args, u32);
                    printUnsigned(n, 16);
                    break;
                }
                case 'c': {
                    char c = va_arg(args, int);
                    printChar(c);
                    break;
                }
                case 'p': {
                    void* n = va_arg(args, void*);
                    printUnsigned((usize)n, 16);
                    break;
                }
                case 's': {
                    const char* s = va_arg(args, const char*);
                    printString(s);
                    break;
                }
                case '%':
                    printChar('%');
                    break;
                default:
                    break;
            }
            seenPercent = false;
        } else {
            switch (*c) {
                case '%':
                    seenPercent = true;
                    break;
                default:
                    printChar(*c);
                    break;
            }
        }
    }
    va_end(args);
}

void printf(const char* fmt, ...) {
    va_list args;
    va_start(args);
    vprintf(fmt, args);
}
