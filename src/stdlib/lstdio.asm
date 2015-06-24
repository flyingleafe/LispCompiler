global printInt

extern printf
extern malloc
extern free

section .text

printInt:
        mov     rdi, printInt_outlabel
        mov     rsi, rax
        call    printf
        ret

section .data

printInt_outlabel:      db '%d', 10, 0
