extern malloc
extern free

global memmgr_alloc
global memmgr_free

;;; MEMORY MANAGEMENT IS UNTESTED CURRENTLY

section .text

;;; Heap:
;;; r13 -- beggining of heap
;;; r14 -- current size in bytes
;;; r15 -- current capacity

;;; Reserves `size` bytes for use and returns pointer to it
;;; void* memmgr_alloc(int size)
memmgr_alloc:
        cmp     r13, 0
        je      .init
        mov     rax, rdi
        add     rax, r14
        cmp     rax, r15
        jg      .realloc
        mov     rax, r13
        add     rax, r14
        add     r14, rdi
        ret

        .init
        mov     rdx, rax
        call    malloc
        ret

        .realloc
        ;; rcx -- wanted size
        ;; rdx -- future size
        mov     rcx, r15

        ;; Calculate new memory size
        .loop
        mov     rax, rdx
        mov     r10, 2
        mul     r10
        cmp     rcx, rdx
        jge     .loop
        mov     rdx, r8

        ;; Allocate new memory chunk
        call    malloc
        cmp     rax, 0
        je      .fail
        mov     r8, rax

        ;; Copy old data
        xor     rcx, rcx
        .cploop
        mov     al, byte[r13+rcx]
        mov     byte[r8+rcx], al
        inc     rcx
        cmp     rcx, r14
        jl      .cploop

        ;; Free old data
        mov     rdx, r13
        call    free

        ;; Set new data pointer
        mov     r13, r9
        ;; Set new data size
        mov     r15, r8

        mov     rax, r9
        jmp     .return
        .fail
        mov     rax, 0
        .return
        ret

;;; Frees allocated buffer.
;;; void memmgr_free()
memmgr_free:
        mov     r13, rdx
        call    free
        xor     r13, r13
        xor     r14, r14
        xor     r15, r15
        ret

memmgr_list:
        ret
