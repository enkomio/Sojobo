#include <stdint.h>
#include <Windows.h>

void copy_code(void *buffer)
{
	__asm 
	{
		jmp start
		code:
		push ebp;
		mov ebp, esp
		xor eax, eax;
		mov edx, 1;
		mov ecx, DWORD PTR [ebp+8];
	l:	
		xadd eax, edx;
		loop l;
		pop ebp
		ret
	start:
		mov esi, code;
		mov edi, buffer;
		mov ecx, start;
		sub ecx, code;
		rep movsb
	}
}


int main()
{
	uint32_t ret_val = 0;
	void *fibonacci = VirtualAlloc(NULL, 0x1000, MEM_COMMIT | MEM_RESERVE, PAGE_EXECUTE_READWRITE);
	copy_code(fibonacci);
	ret_val = ((uint32_t (*)(uint32_t))fibonacci)(6);
	VirtualFree(fibonacci, 0x1000, MEM_RELEASE);
	return 0;
}