segment	.text
align	4
global	_main:function
_main:
	push	ebp
	mov	ebp, esp
	sub	esp, 0
	push	dword 3
	push	dword [esp]
segment	.data
align	4
x:
	dd	0
segment	.text
	push	dword $x
	pop	ecx
	pop	eax
	mov	[ecx], eax
	add	esp, 4
	push	dword $x
	pop	eax
	push	dword [eax]
	push	dword 2
	pop	eax
	add	dword [esp], eax
	push	dword [esp]
segment	.data
align	4
y:
	dd	0
segment	.text
	push	dword $y
	pop	ecx
	pop	eax
	mov	[ecx], eax
	add	esp, 4
	push	dword $y
	pop	eax
	push	dword [eax]
	call	printi
	add	esp, 4
	call	println
	push	dword 0
	pop	eax
	leave
	ret
extern	readi
extern	printi
extern	prints
extern	println
