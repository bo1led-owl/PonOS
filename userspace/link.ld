SECTIONS {
    . = 0x800000;
    .startup : {
        build/userspace/startup.nasm.o (.text);
    }

    .userspace : {
        *(.text);
        *(.rodata);
        *(.rodata.*);
        *(.data);
        *(.bss);
    }

    /DISCARD/ : {
        *(.eh_frame)
        *(.note.gnu.property)
        *(.comment)
    }
}
