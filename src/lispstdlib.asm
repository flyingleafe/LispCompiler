global printInt

extern printf
extern malloc

section .text

printInt:
        mov     rdi, printInt_outlabel
        mov     rsi, rax
        call    printf
        ret

;;; Heap:
;;; r13 -- beggining of heap
;;; r14 -- current size in bytes
;;; r15 -- current capacity

;;; void* memmgr_alloc(int size)
memmgr_alloc:
        cmp     r13, 0
        je      .memmgr_init
        mov     rax, rdi
        add     rax, r14
        cmp     rax, r15
        jg      .realloc
        mov     rax, r13
        add     rax, r14
        add     r14, rdi
        ret

        .memmgr_init
        mov     rdx, rax
        call    malloc
        ret

        .realloc
        ;; TBD
        ret

section .data

printInt_outlabel:      db '%d', 10, 0
