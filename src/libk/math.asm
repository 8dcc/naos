section .text

; double sqrt(double x);
global sqrt:function
sqrt:
    push    ebp
    mov     ebp, esp
    sub     esp, 8                      ; Reserve 8 bytes on the stack

    movsd   xmm0, qword [ebp + 8]       ; Move double argument to xmm0
    sqrtsd  xmm0, xmm0                  ; xmm0 = sqrt(xmm0);
    movsd   qword [ebp - 8], xmm0       ; Move xmm0 to reserved bytes
    fld     qword [ebp - 8]             ; Push reserved bytes to FPU stack

    mov     esp, ebp
    pop     ebp
    ret

; double sin(double x);
global sin:function
sin:
    push    ebp
    mov     ebp, esp
    sub     esp, 8                      ; Reserve 8 bytes on the stack

    fld     qword [ebp + 8]             ; Push to FPU stack
    fsin                                ; st(0) = sin(st(0));
    fstp    qword [ebp - 8]             ; Move st(0) to reserved bytes
    fld     qword [ebp - 8]             ; Push reserved bytes to FPU stack

    mov     esp, ebp
    pop     ebp
    ret

; double cos(double x);
global cos:function
cos:
    push    ebp
    mov     ebp, esp
    sub     esp, 8

    fld     qword [ebp + 8]
    fcos
    fstp    qword [ebp + 8]
    fld     qword [ebp - 8]

    mov     esp, ebp
    pop     ebp
    ret

; double tan(double x);
global tan:function
tan:
    push    ebp
    mov     ebp, esp
    sub     esp, 8

    fld     qword [ebp + 8]
    fsincos
    fdivp   st1, st0                    ; st(1) /= st(0)
    fstp    qword [ebp + 8]

    mov     esp, ebp
    pop     ebp
    ret

; double cot(double x);
global cot:function
cot:
    push    ebp
    mov     ebp, esp
    sub     esp, 8

    fld     qword [ebp + 8]
    fsincos
    fdivrp  st1, st0                    ; st(0) = st(1) / st(0)
    fstp    qword [ebp + 8]

    mov     esp, ebp
    pop     ebp
    ret
