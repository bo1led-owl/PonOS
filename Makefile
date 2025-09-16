PAYLOAD := ./payload
PAYLOAD_SIZE := $(shell wc -c < $(PAYLOAD))
IMG := build/boot.img
QEMU_FLAGS := -cpu pentium2 -m 4g -monitor stdio -device VGA -drive file=$(IMG),format=raw,if=floppy

run: $(IMG)
	qemu-system-x86_64 $(QEMU_FLAGS)

debug: $(IMG)
	qemu-system-x86_64 $(QEMU_FLAGS) -s -S & lldb --local-lldbinit

build/main.bin: build src/loader.nasm
	nasm -fbin -dPAYLOAD_SIZE=$(PAYLOAD_SIZE) src/loader.nasm -o build/main.bin

$(IMG): build/main.bin $(PAYLOAD)
	dd if=/dev/zero of=$@ bs=1024 count=1440
	dd if=$< of=$@ conv=notrunc
	dd if=$(PAYLOAD) of=$@ conv=notrunc seek=1

build:
	mkdir build

.PHONY: run
