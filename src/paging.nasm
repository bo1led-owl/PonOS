bits 32

global setupPaging
setupPaging:
    mov eax, cr4
    or eax, (1 << 4)
    mov cr4, eax
    mov eax, cr0
    and eax, ~(1 << 16)
    mov cr0, eax
    mov eax, [esp + 4]
    mov cr3, eax
    ret

global enablePaging
enablePaging:
    mov eax, cr0
    or eax, (1 << 31)
    mov cr0, eax
    ret

global disablePaging
disablePaging:
    mov eax, cr0
    and eax, ~(1 << 31)
    mov cr0, eax
    ret
