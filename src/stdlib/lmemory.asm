extern malloc
extern realloc
extern free

global memmgr_init
global memmgr_alloc
global memmgr_free
global lcons

;;; MEMORY MANAGEMENT IS UNTESTED CURRENTLY

section .text

;;; Heap:
;;; r9  -- beggining of heap
;;; r10 -- current size in bytes
;;; r11 -- current capacity

memmgr_init:
        xor     r9, r9
        xor     r10, r10
        xor     r11, r11
        ret

;;; Reserves `size` bytes for use and returns pointer to it
;;; void* memmgr_alloc(int size);
memmgr_alloc:
        cmp     r9, 0
        je      .init
        mov     rax, rdi
        add     rax, r10
        cmp     rax, r11
        jg      .realloc
        mov     rax, r9
        add     rax, r10
        add     r10, rdi
        ret

        ;; Initialize memory with first chunk
        .init
        xor     rax, rax
        push    rdi
        call    malloc
        cmp     rax, 0
        je      .fail
        pop     r11
        mov     r10, r11
        mov     r9, rax
        ret

        .realloc

        ;; Save current size
        mov     rcx, r10

        ;; r10 -- wanted size
        ;; rax -- future capacity
        ;; r8 -- expand coefficient
        mov     r10, rax
        mov     rax, r11
        mov     r8, 2

        ;; Calculate new memory size
        .loop
        mul     r8
        cmp     r10, rax
        jge     .loop

        ;; Save new capacity
        mov     r11, rax

        ;; Allocate new memory chunk
        push    r9
        push    r10
        push    r11
        push    rcx
        mov     rdi, r9
        mov     rsi, r11
        xor     rax, rax
        call    realloc
        pop     rcx
        pop     r11
        pop     r10
        pop     r9
        cmp     rax, 0
        je      .fail

        ;; No need to free -- realloc does

        ;; Set r9 to new memory
        mov     r9, rax

        ;; Allocate address to new memory + prevsize offset
        mov     rax, r9
        add     rax, rcx

        jmp     .return
        .fail
        mov     rax, 0
        .return
        ret

;;; Frees allocated buffer.
;;; void memmgr_free();
memmgr_free:
        mov     rdi, r9
        xor     rax, rax
        call    free
        xor     r13, r13
        xor     r14, r14
        xor     r15, r15
        ret

;;; void* memmgr_cons(void* elem, void* list);
lcons:
        push    rdi
        push    rsi
        mov     rdi, 16
        call    memmgr_alloc
        pop     rsi
        pop     rdi
        mov     qword[rax], rdi
        mov     qword[rax+8], rsi
        ret
