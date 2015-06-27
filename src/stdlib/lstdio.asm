global printInt

extern printf
extern malloc
extern free

section .text

;;; Prints int to stdout
;;; void printInt(int a);
printInt:
        push    r9
        push    r10
        push    r11

        mov     rsi, rdi
        mov     rdi, printf_int_n
        xor     rax, rax
        call    printf

        pop     r11
        pop     r10
        pop     r9
        ret

;;; void printList(void* a);
printList:
        push    r9
        push    r10
        push    r11

        push    rdi
        mov     rdi, printf_list_lb
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
        mov     rdi, printf_int
        xor     rax, rax
        call    printf
        pop     rdi

        ;; exit if second elem is nil
        cmp     qword[rdi+8], 0
        je      .end

        push    rdi
        mov     rdi, printf_comma_sp
        xor     rax, rax
        call    printf
        pop     rdi

        ;; set list to it's tail and print tail
        mov     rdi, qword[rdi+8]
        jmp     .loop


        .end
        mov     rdi, printf_list_rb
        xor     rax, rax
        call    printf

        pop     r11
        pop     r10
        pop     r9

section .data

printf_list_lb:         db '[', 0
printf_list_rb:         db ']', 0
printf_int:             db '%d', 0
printf_int_n:           db '%d', 10, 0
printf_n:               db 10, 0
printf_comma_sp:        db ',', 0
