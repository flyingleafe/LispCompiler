extern printf
global printInt

section .text

printInt:
        mov     rdx, 1
        mov     rsi, printInt_outlabel
        mov     rdx, rax
        call    printf
        ret

section .data

printInt_outlabel:      db "%d", 0
