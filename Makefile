run: build/boot.img
	qemu-system-x86_64 -cpu pentium2 -m 1g -fda $^ -monitor stdio -device VGA

debug: build/boot.img
	qemu-system-x86_64 -cpu pentium2 -m 1g -fda $^ -monitor stdio -device VGA -s -S &
	gdb

build/main.bin: build src/main.nasm
	nasm -fbin src/main.nasm -o build/main.bin

build/boot.img: build/main.bin
	dd if=/dev/zero of=build/boot.img bs=1024 count=1440
	dd if=build/main.bin of=build/boot.img conv=notrunc

build:
	mkdir build

.PHONY: run
