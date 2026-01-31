	.intel_syntax noprefix
	.text
	# Standard Library Functions
print:
	push	rbp
	mov	rbp, rsp
	push	rbx
	push	r12
	sub	rsp, 32
	mov	rax, rdi
	cmp	rax, 0x1000
	jae	.Lprint_string
.Lprint_int:
	mov	rax, rdi
	mov	rcx, 10
	lea	rsi, [rbp - 1]
	mov	byte ptr [rsi], 10
	test	rax, rax
	jns	.Lprint_positive
	neg	rax
	mov	r12, 1
	jmp	.Lprint_convert
.Lprint_positive:
	xor	r12, r12
.Lprint_convert:
	test	rax, rax
	jnz	.Lprint_loop
	dec	rsi
	mov	byte ptr [rsi], 48
	jmp	.Lprint_sign
.Lprint_loop:
	test	rax, rax
	jz	.Lprint_sign
	dec	rsi
	xor	rdx, rdx
	div	rcx
	add	dl, 48
	mov	[rsi], dl
	jmp	.Lprint_loop
.Lprint_sign:
	test	r12, r12
	jz	.Lprint_write
	dec	rsi
	mov	byte ptr [rsi], 45
	jmp	.Lprint_write
.Lprint_string:
	mov	rsi, rdi
	xor	rdx, rdx
.Lstrlen_loop:
	cmp	byte ptr [rsi + rdx], 0
	je	.Lstrlen_done
	inc	rdx
	jmp	.Lstrlen_loop
.Lstrlen_done:
	test	rdx, rdx
	jz	.Lprint_done
	mov	byte ptr [rsi + rdx], 10
	inc	rdx
	jmp	.Lprint_write
.Lprint_write:
	mov	rax, 1
	mov	rdi, 1
	lea	rdx, [rbp - 1]
	sub	rdx, rsi
	inc	rdx
	syscall
.Lprint_done:
	add	rsp, 32
	pop	r12
	pop	rbx
	pop	rbp
	ret
len:
	push	rbp
	mov	rbp, rsp
	xor	rax, rax
.Llen_loop:
	cmp	byte ptr [rdi + rax], 0
	je	.Llen_done
	inc	rax
	jmp	.Llen_loop
.Llen_done:
	pop	rbp
	ret
test:
	push	rbp
	mov	rbp, rsp
	mov	QWORD PTR [rbp - 8], rdi
	mov	QWORD PTR [rbp - 16], rsi
	mov	QWORD PTR [rbp - 24], rdx
	sub	rsp, 24
	push	rdi
	push	rsi
	push	rdx
	push	rcx
	push	r8
	push	r9
	lea	rax, [rip + .Lstr_0]
	mov	rdi, rax
	call	print
	pop	r9
	pop	r8
	pop	rcx
	pop	rdx
	pop	rsi
	pop	rdi
	push	rdi
	push	rsi
	push	rdx
	push	rcx
	push	r8
	push	r9
	push	rdi
	push	rsi
	push	rdx
	push	rcx
	push	r8
	push	r9
	lea	rax, [rip + .Lstr_0]
	mov	rdi, rax
	call	len
	pop	r9
	pop	r8
	pop	rcx
	pop	rdx
	pop	rsi
	pop	rdi
	mov	rdi, rax
	call	print
	pop	r9
	pop	r8
	pop	rcx
	pop	rdx
	pop	rsi
	pop	rdi
	mov	rax, QWORD PTR [rbp - 24]
	mov	rsp, rbp
	pop	rbp
	ret
	mov	rsp, rbp
	pop	rbp
	ret
	.globl main
main:
	push	rbp
	mov	rbp, rsp
	mov	rax, 42
	mov	QWORD PTR [rbp - 8], rax
	mov	-16[rbp], rax
	mov	rax, 10
	push	rax
	mov	rax, QWORD PTR [rbp - 8]
	pop	rbx
	sub	rax, rbx
	mov	QWORD PTR [rbp - 24], rax
	mov	-32[rbp], rax
	mov	rax, QWORD PTR [rbp - 24]
	push	rax
	mov	rax, QWORD PTR [rbp - 8]
	pop	rbx
	add	rax, rbx
	mov	QWORD PTR [rbp - 40], rax
	mov	-48[rbp], rax
	push	rdi
	push	rsi
	push	rdx
	push	rcx
	push	r8
	push	r9
	mov	rax, QWORD PTR [rbp - 40]
	mov	rdi, rax
	call	print
	pop	r9
	pop	r8
	pop	rcx
	pop	rdx
	pop	rsi
	pop	rdi
	push	rdi
	push	rsi
	push	rdx
	push	rcx
	push	r8
	push	r9
	mov	rax, QWORD PTR [rbp - 8]
	mov	rdi, rax
	call	print
	pop	r9
	pop	r8
	pop	rcx
	pop	rdx
	pop	rsi
	pop	rdi
	mov	rax, 0
	test	rax, rax
	jz	.Lif_else_1
	push	rdi
	push	rsi
	push	rdx
	push	rcx
	push	r8
	push	r9
	lea	rax, [rip + .Lstr_3]
	mov	rdi, rax
	call	print
	pop	r9
	pop	r8
	pop	rcx
	pop	rdx
	pop	rsi
	pop	rdi
	jmp	.Lif_end_2
.Lif_else_1:
	mov	rax, 0
	test	rax, rax
	jz	.Lif_else_4
	push	rdi
	push	rsi
	push	rdx
	push	rcx
	push	r8
	push	r9
	lea	rax, [rip + .Lstr_6]
	mov	rdi, rax
	call	print
	pop	r9
	pop	r8
	pop	rcx
	pop	rdx
	pop	rsi
	pop	rdi
	jmp	.Lif_end_5
.Lif_else_4:
	push	rdi
	push	rsi
	push	rdx
	push	rcx
	push	r8
	push	r9
	lea	rax, [rip + .Lstr_7]
	mov	rdi, rax
	call	print
	pop	r9
	pop	r8
	pop	rcx
	pop	rdx
	pop	rsi
	pop	rdi
.Lif_end_5:
.Lif_end_2:
	push	rdi
	push	rsi
	push	rdx
	push	rcx
	push	r8
	push	r9
	push	rdi
	push	rsi
	push	rdx
	push	rcx
	push	r8
	push	r9
	mov	rax, 1
	mov	rdi, rax
	mov	rax, 2
	mov	rsi, rax
	mov	rax, 3
	mov	rdx, rax
	call	test
	pop	r9
	pop	r8
	pop	rcx
	pop	rdx
	pop	rsi
	pop	rdi
	mov	rdi, rax
	call	print
	pop	r9
	pop	r8
	pop	rcx
	pop	rdx
	pop	rsi
	pop	rdi
	xor	rax, rax
	mov	rsp, rbp
	pop	rbp
	ret

	.section	.rodata
.Lstr_0:
	.string "testing"
.Lstr_7:
	.string "WHATTTT"
.Lstr_3:
	.string "i love you"
.Lstr_6:
	.string "yo"

	.section	.note.GNU-stack,"",@progbits
