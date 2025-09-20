IMG := build/boot.img
KERNEL := build/kernel.o
LOADER := build/loader.o
KERNEL_SIZE_KB := 40
KERNEL_BIN := build/kernel.bin
QEMU_FLAGS := -cpu pentium2 -m 4g -monitor stdio -device VGA -drive file=$(IMG),format=raw,if=floppy

C_SOURCES := $(wildcard src/*.c)
C_HEADERS := $(wildcard src/*.h)
CCFLAGS := -std=c23 -m32 -ffreestanding -fno-pie -mno-sse -fno-stack-protector -Os -c

run: $(IMG)
	qemu-system-i386 $(QEMU_FLAGS)

debug: $(IMG)
	qemu-system-i386 $(QEMU_FLAGS) -s -S & lldb --local-lldbinit

$(LOADER): build src/loader.nasm
	nasm -felf32 -dKERNEL_SIZE_KB=$(KERNEL_SIZE_KB) src/loader.nasm -o $@

$(KERNEL): build $(C_HEADERS) $(C_SOURCES)
	$(CC) $(CCFLAGS) -c $(C_SOURCES) -o $@

$(KERNEL_BIN): build $(LOADER) $(KERNEL)
	ld.lld -e kernelEntry $(LOADER) $(KERNEL) -T link.ld -o build/kernel.elf
	objcopy -I elf32-i386 -O binary build/kernel.elf $@

$(IMG): $(KERNEL_BIN)
	dd if=/dev/zero of=$@ bs=1024 count=1440
	dd if=$< of=$@ conv=notrunc

build:
	mkdir build

clean:
	rm -fr build

.PHONY: run debug clean
