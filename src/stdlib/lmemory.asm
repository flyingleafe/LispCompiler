extern malloc
extern realloc
extern free

global memmgr_init
global memmgr_alloc
global memmgr_free
global memmgr_cons
global memmgr_make_closure

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

;;; Reserves `size` bytes for use and returns pointer to it
;;; void* memmgr_alloc(int size);
memmgr_alloc:
        cmp     r12, 0
        je      .init
        mov     rax, rdi
        add     rax, r13
        cmp     rax, r14
        jg      .realloc
        mov     rax, r12
        add     rax, r13
        add     r13, rdi
        ret

        ;; Initialize memory with first chunk
        .init
        xor     rax, rax
        push    rdi
        call    malloc
        cmp     rax, 0
        je      .fail
        pop     r14
        mov     r13, r14
        mov     r12, rax
        ret

        .realloc

        ;; Save current size
        mov     rcx, r13

        ;; r13 -- wanted size
        ;; rax -- future capacity
        ;; r8 -- expand coefficient
        mov     r13, rax
        mov     rax, r14
        mov     r8, 2

        ;; Calculate new memory size
        .loop
        mul     r8
        cmp     r13, rax
        jge     .loop

        ;; Save new capacity
        mov     r14, rax

        ;; Allocate new memory chunk
        push    r12
        push    r13
        push    r14
        push    rcx
        mov     rdi, r12
        mov     rsi, r14
        xor     rax, rax
        call    realloc
        pop     rcx
        pop     r14
        pop     r13
        pop     r12
        cmp     rax, 0
        je      .fail

        ;; No need to free -- realloc does

        ;; Set r12 to new memory
        mov     r12, rax

        ;; Allocate address to new memory + prevsize offset
        mov     rax, r12
        add     rax, rcx

        jmp     .return
        .fail
        mov     rax, 0
        .return
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

;;; void* memmgr_cons(void* elem, void* list);
memmgr_cons:
        push    rdi
        push    rsi
        mov     rdi, 16
        call    memmgr_alloc
        pop     rsi
        pop     rdi
        mov     qword[rax], rdi
        mov     qword[rax+8], rsi
        ret

;;; void* memmgr_make_closure(void* func, int argc, void* args...);
memmgr_make_closure:
        push    rdi
        push    rsi
        push    rdx
        push    rcx
        push    r8
        push    r9
        mov     rdi, rsi
        shl     rdi, 3          ; ×8
        add     rdi, 8
        call    memmgr_alloc
        pop     r9
        pop     r8
        pop     rcx
        pop     rdx
        pop     rsi
        pop     rdi

        ;; Registers args
        mov     r10, rax
        mov     qword[r10], rdi
        add     r10, 8

        xor     r11, r11
        cmp     r11, rsi
        jge     .end

        mov     qword[r10], rdx
        add     r10, 8
        inc     r11
        cmp     r11, rsi
        jge     .end

        mov     qword[r10], rcx
        add     r10, 8
        inc     r11
        cmp     r11, rsi
        jge     .end

        mov     qword[r10], r8
        add     r10, 8
        inc     r11
        cmp     r11, rsi
        jge     .end

        mov     qword[r10], r9
        add     r10, 8
        inc     r11
        cmp     r11, rsi
        jge     .end

        ;; Copying stack args
        xor     r8, r8
        mov     r8, rbp
        add     r8, 16
        .loop
        mov     rcx, qword[r8]
        mov     qword[r10], rcx
        add     r10, 8
        add     r8, 8
        inc     r11
        cmp     r11, rsi
        jl      .loop

        .end
        ret
