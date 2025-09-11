QEMU_FLAGS := -cpu pentium2 -m 1g -monitor stdio -device VGA

run: build/boot.img
	qemu-system-x86_64 $(QEMU_FLAGS) -drive file=$<,format=raw

debug: build/boot.img
	qemu-system-x86_64 $(QEMU_FLAGS) -drive file=$<,format=raw -s -S & gdb
	killall qemu-system-x86_64 

build/main.bin: build src/main.nasm
	nasm -fbin src/main.nasm -o build/main.bin

build/boot.img: build/main.bin payload
	dd if=/dev/zero of=$@ bs=1024 count=1440
	dd if=$< of=$@ conv=notrunc
	dd if=payload of=$@ conv=notrunc seek=1

build:
	mkdir build

.PHONY: run
