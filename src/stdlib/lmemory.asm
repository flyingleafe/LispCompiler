extern malloc
extern realloc
extern scanf

global memmgr_alloc
global memmgr_make_closure
global memmgr_cons
;;; global memmgr_allocList
global readString

section .text

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

;;; returns list of wanted length, filled with zeroes
;;; void* memmgr_alloclist(int length);
memmgr_allocList:
        mov     rcx, rdi
        mov     rax, 0
        .loop
        mov     rdi, 0
        mov     rsi, rax
        push    rcx
        xor     rax, rax
        call    memmgr_cons
        pop     rcx
        dec     rcx
        cmp     rcx, 0
        jg      .loop

        ret

;;; reads string, returns it as list of chars (allocated)
;;; void *readString();
readString:
        sub     rsp, 1024        ; maximum supported string length
        mov     rsi, rsp
        mov     rdi, pattern_str
        xor     rax, rax
        call    scanf

        ;; count length of list
        xor     rcx, rcx
        .loop1
        mov     rax, rsp
        add     rax, rcx
        cmp     byte[rax], 0
        je      .loop1_end
        inc     rcx
        cmp     rcx, 1024
        je      .fail
        jmp     .loop1
        .loop1_end

        ;; allocate list of needed size
        mov     rdi, rcx
        call    memmgr_allocList

        ;; copy string to list
        push    rax
        mov     rcx, 8
        .loop2
        cmp     rax, 0
        je      .loop2_end
        mov     rdx, rsp
        add     rdx, rcx
        mov     dl, byte[rdx]
        mov     byte[rax], dl
        mov     rax, qword[rax+8]
        inc     rcx
        jmp     .loop2
        .loop2_end

        pop     rax
        add     rsp, 1024
        jmp     .return
        .fail
        mov     dword[0], 0
        .return
        ret


section .data

pattern_str:            db '%s', 0
