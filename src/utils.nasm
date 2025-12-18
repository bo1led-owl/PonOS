bits 32

global infiniteLoop
infiniteLoop:
    jmp infiniteLoop

global infiniteRecursion
infiniteRecursion:
    sub esp, 4092
    call infiniteRecursion
