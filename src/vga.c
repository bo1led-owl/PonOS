#include "vga.h"

#include "assert.h"
#include "mem.h"
#include "panic.h"

typedef u16 VgaChar;

static VgaChar vgaChar(Color bg, Color fg, char c) {
    return (bg << 12) | (fg << 8) | ((u8)c & 0xFF);
}

static VgaChar* mainBuffer = (VgaChar*)0xB8000;

typedef struct Window {
    VgaChar* buffer;
    Color bg, fg;
    usize x, y;
    usize rows, columns;
} Window;

constexpr usize MAX_WINDOWS = 32;
static bool initialized = false;
static Window windows[MAX_WINDOWS] = {};
static usize nWindows = 0;

static Window mw = (Window){
    .bg = Color_Black,
    .fg = Color_White,
    .buffer = (VgaChar*)0xB8000,
    .rows = VGA_ROWS,
    .columns = VGA_COLUMNS,
    .x = 0,
    .y = 0,
};

WindowHandle mainWindow() {
    return &mw;
}

static void putcharRaw(WindowHandle w, char c, usize x, usize y) {
    *(w->buffer + y * VGA_COLUMNS + x) = vgaChar(w->bg, w->fg, c);
}

static void scroll(WindowHandle w) {
    for (usize i = 1; i < w->rows; ++i) {
        memcpy(w->buffer + VGA_COLUMNS * (i - 1), w->buffer + VGA_COLUMNS * i,
               w->columns * sizeof(VgaChar));
    }

    for (usize i = 0; i < w->columns; ++i) {
        putcharRaw(w, 0, i, w->rows - 1);
    }
}

void clearWindow(WindowHandle w) {
    for (usize i = 0; i < w->rows; ++i) {
        for (usize j = 0; j < w->columns; ++j) {
            putcharRaw(w, 0, j, i);
        }
    }
    w->x = w->y = 0;
}

void initScreen() {
    for (usize i = 0; i < nWindows; ++i) {
        clearWindow(windows + i);
    }
    initialized = true;
}

WindowHandle addWindow(usize startX, usize startY, usize rows, usize columns) {
    assert(!initialized && "All windows must be added before `initScreen` is called");
    assert(nWindows < MAX_WINDOWS);
    WindowHandle res = &windows[nWindows++];
    *res = (Window){
        .buffer = mainBuffer + startY * VGA_COLUMNS + startX,
        .columns = columns,
        .rows = rows,
        .bg = Color_Black,
        .fg = Color_White,
        .x = 0,
        .y = 0,
    };
    return res;
}

void setBgColor(WindowHandle w, Color c) {
    w->bg = c;
}

void setFgColor(WindowHandle w, Color c) {
    w->fg = c;
}

static void fixScreen(WindowHandle w) {
    if (w->x >= w->columns) {
        w->y += 1;
        w->x = 0;
    }

    while (w->y >= w->rows) {
        scroll(w);
        w->y -= 1;
    }
}

void putchar(WindowHandle w, char c) {
    if (c == '\n') {
        w->y += 1;
        w->x = 0;
    } else if (c == '\r') {
        w->x = 0;
        return;
    } else {
        putcharRaw(w, c, w->x, w->y);
        w->x += 1;
    }
    fixScreen(w);
}

static void printString(WindowHandle w, const char* s) {
    for (const char* c = s; *c; ++c) {
        putchar(w, *c);
    }
}

static void printUnsigned(WindowHandle w, u32 n, u8 radix) {
    assert(radix > 1);

    if (n == 0) {
        putchar(w, '0');
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

    assert(i < 33);
    buf[i] = 0;

    for (usize j = 0; j < i / 2; ++j) {
        char tmp = buf[j];
        buf[j] = buf[i - j - 1];
        buf[i - j - 1] = tmp;
    }

    printString(w, buf);
}

static void printSigned(WindowHandle w, i32 n, u8 radix) {
    if (n < 0) {
        putchar(w, '-');
        n *= -1;
    }

    printUnsigned(w, n, radix);
}

void vprintf(WindowHandle w, const char* fmt, va_list args) {
    bool seenPercent = false;
    for (const char* c = fmt; *c; ++c) {
        if (seenPercent) {
            switch (*c) {
                case 'd': {
                    i32 n = va_arg(args, i32);
                    printSigned(w, n, 10);
                    break;
                }
                case 'u': {
                    u32 n = va_arg(args, u32);
                    printUnsigned(w, n, 10);
                    break;
                }
                case 'b': {
                    u32 n = va_arg(args, u32);
                    printUnsigned(w, n, 2);
                    break;
                }
                case 'x': {
                    u32 n = va_arg(args, u32);
                    printUnsigned(w, n, 16);
                    break;
                }
                case 'c': {
                    char c = va_arg(args, int);
                    putchar(w, c);
                    break;
                }
                case 'p': {
                    void* n = va_arg(args, void*);
                    printUnsigned(w, (usize)n, 16);
                    break;
                }
                case 's': {
                    const char* s = va_arg(args, const char*);
                    printString(w, s);
                    break;
                }
                case '%':
                    putchar(w, '%');
                    break;
                default:
                    panic("unknown type specifier %c\n", *c);
            }
            seenPercent = false;
        } else {
            switch (*c) {
                case '%':
                    seenPercent = true;
                    break;
                default:
                    putchar(w, *c);
                    break;
            }
        }
    }
    va_end(args);
}

void printf(WindowHandle w, const char* fmt, ...) {
    va_list args;
    va_start(args);
    vprintf(w, fmt, args);
}
