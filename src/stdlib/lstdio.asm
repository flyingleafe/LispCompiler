global printInt

extern printf
extern malloc
extern free

section .text

;;; Prints int to stdout
;;; void printInt(int a);
printInt:
        mov     rsi, rdi
        mov     rdi, printInt_outlabel
        xor     rax, rax
        call    printf
        ret

section .data

printInt_outlabel:      db '%d', 10, 0
