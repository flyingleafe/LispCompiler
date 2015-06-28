extern free

global memmgr_free
global memmgr_init

section .text

;;; Heap:
;;; r12  -- beggining of heap
;;; r13 -- current size in bytes
;;; r14 -- current capacity
memmgr_init:
        xor     r12, r12
        xor     r13, r13
        xor     r14, r14
        xor     rax, rax
        ret

;;; Frees allocated buffer.
;;; void memmgr_free();
memmgr_free:
        mov     rdi, r12
        xor     rax, rax
        call    free
        xor     r13, r13
        xor     r14, r14
        xor     r15, r15
        xor     rax, rax
        ret
