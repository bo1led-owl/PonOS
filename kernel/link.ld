SECTIONS {
    . = 0x7C00;
    .boot : {
        build/kernel/loader.o (.text);
    }

    .kernel : {
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
