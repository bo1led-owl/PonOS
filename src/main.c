#include "utils.h"

void kernelEntry() {
    *((short int*)0xB8000) = 0;
    infiniteLoop();
}
