#include "vga.h"

#include "assert.h"
#include "mem.h"
#include "panic.h"

typedef u16 VgaChar;

static VgaChar vgaChar(Color bg, Color fg, char c) {
    return (bg << 12) | (fg << 8) | ((u8)c & 0xFF);
}

static Color bg_color = Color_Black;
static Color fg_color = Color_White;

static usize x = 0;
static usize y = 0;
static VgaChar* buffer = (VgaChar*)0xB8000;

constexpr usize COLUMNS = 80;
constexpr usize ROWS = 25;
constexpr usize BUF_SIZE = (COLUMNS * ROWS * sizeof(VgaChar));

static void putcharRaw(char c, usize x, usize y) {
    *(buffer + COLUMNS * y + x) = vgaChar(bg_color, fg_color, c);
}

static void vgaScroll() {
    memmove(buffer, buffer + COLUMNS, BUF_SIZE - COLUMNS * sizeof(VgaChar));

    for (usize j = 0; j < COLUMNS; ++j) {
        putcharRaw(0, j, ROWS - 1);
    }
}

void vgaInit() {
    memzero(buffer, BUF_SIZE);
    x = y = 0;
}

void vgaClear() {
    for (usize i = 0; i < ROWS; ++i) {
        for (usize j = 0; j < COLUMNS; ++j) {
            putcharRaw(0, j, i);
        }
    }

    x = y = 0;
}

void vgaSetBgColor(Color c) {
    bg_color = c;
}

void vgaSetFgColor(Color c) {
    fg_color = c;
}

static void fixScreen() {
    if (x >= COLUMNS) {
        y += 1;
        x = 0;
    }

    while (y >= ROWS) {
        vgaScroll();
        y -= 1;
    }
}

void putchar(char c) {
    if (c == '\n') {
        y += 1;
        x = 0;
        fixScreen();
        return;
    } else if (c == '\r') {
        x = 0;
        return;
    }

    putcharRaw(c, x, y);
    x += 1;
    fixScreen();
}

static void printString(const char* s) {
    for (const char* c = s; *c; ++c) {
        putchar(*c);
    }
}

static void printUnsigned(u32 n, u8 radix) {
    assert(radix > 1);

    if (n == 0) {
        putchar('0');
        return;
    }

    // worst case is binary, when we put a char for each of 32 bits
    char buf[33];

    usize i = 0;
    while (n > 0) {
        u32 digit = n % radix;
        char formatted_digit = (digit < 10) ? '0' + digit : 'a' + (digit - 10);
        buf[i] = formatted_digit;
        i += 1;
        n /= radix;
    }

    assert(i < 33);
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
        putchar('-');
        n *= -1;
    }

    printUnsigned(n, radix);
}

void vprintf(const char* fmt, va_list args) {
    bool seen_percent = false;
    bool seen_slash = false;
    for (const char* c = fmt; *c; ++c) {
        // %d %x %c %s \n \r

        if (seen_percent) {
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
                case 'x': {
                    u32 n = va_arg(args, u32);
                    printUnsigned(n, 16);
                    break;
                }
                case 'c': {
                    char c = va_arg(args, int);
                    putchar(c);
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
                    putchar('%');
                    break;
                default:
                    panic("unknown type specifier %c\n", *c);
            }
            seen_percent = false;
        } else if (seen_slash) {
            switch (*c) {
                case 'r':
                    putchar('\r');
                    break;
                case 'n':
                    putchar('\n');
                    break;
                case '\\':
                    putchar('\\');
                    break;
                default:
                    panic("unknown escape sequence \\%c", *c);
            }
            seen_slash = false;
        } else {
            switch (*c) {
                case '%':
                    seen_percent = true;
                    break;
                case '\\':
                    seen_slash = true;
                    break;
                default:
                    putchar(*c);
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
