global printInt
global printList
global readInt

extern printf
extern scanf
extern malloc
extern free

section .text

;;; Prints int to stdout
;;; void printInt(int a);
printInt:
        mov     rsi, rdi
        mov     rdi, pattern_int_n
        xor     rax, rax
        call    printf
        ret

;;; void printList(void* a);
printList:
        push    rdi
        mov     rdi, pattern_list_lb
        xor     rax, rax
        call    printf
        pop     rdi

        ;; empty list
        cmp     rax, 0
        je      .end


        .loop
        mov     rsi, qword[rdi]

        ;; print current elem
        push    rdi
        mov     rdi, pattern_int
        xor     rax, rax
        call    printf
        pop     rdi

        ;; exit if second elem is nil
        cmp     qword[rdi+8], 0
        je      .end

        push    rdi
        mov     rdi, pattern_space
        xor     rax, rax
        call    printf
        pop     rdi

        ;; set list to it's tail and print tail
        mov     rdi, qword[rdi+8]
        jmp     .loop


        .end
        mov     rdi, pattern_list_rb
        xor     rax, rax
        call    printf

        mov     rdi, pattern_n
        xor     rax, rax
        call    printf

        xor     rax, rax
        ret

;;; Blocks
;;; int readInt();
readInt:
        sub     rsp, 8
        mov     rsi, rsp
        mov     rdi, pattern_int
        xor     rax, rax
        call    scanf
        add     rsp, 8

        mov     rax, qword[rsp-8]
        ret

section .data

pattern_list_lb:         db '(', 0
pattern_list_rb:         db ')', 0
pattern_int:             db '%d', 0
pattern_int_n:           db '%d', 10, 0
pattern_n:               db 10, 0
pattern_comma_sp:        db ',', 0
pattern_space:           db ' ', 0
