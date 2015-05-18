.globl bedrock_stack_size
# Assume the C application has defined here the stack size (in words) for each new thread.

.globl bedrock_heap
.equ heapSize, 1024 * 1024
.equ bedrock_stack, bedrock_heap + heapSize * 4
.equ globalSched, bedrock_stack + 50 * 4
.equ globalSp, globalSched + 4

.macro PUSHA
   push %rbx
   push %rbp
   push %rdi
   push %rsi
   push %r8
   push %r9
   push %r10
   push %r11
   push %r12
   push %r13
   push %r14
   push %r15
.endm

.macro POPA
   pop %r15
   pop %r14
   pop %r13
   pop %r12
   pop %r11
   pop %r10
   pop %r9
   pop %r8
   pop %rsi
   pop %rdi
   pop %rbp
   pop %rbx
.endm
        
# This is where the RTOS system jumps when it's ready to turn over control to the application.
        .globl rtos_main
        .globl app_main
        .global scheduler_exit
app_main:
        # Save starting stack pointer.
        movl %ebx, globalSp
        # Initialize scheduler data structure.
        movl $app_main0, %esi # Bedrock return pointer
        jmp scheduler_init
app_main0:
        call rtos_main
        # Now exit the current "thread".
        movl $bedrock_stack, %ebx
        movl $50, (%ebx)
        jmp scheduler_exit

# WRAPPERS FOR THREAD LIBRARY FUNCTIONS

return:
        ret
        
        .globl scheduler_spawn
        .globl bedrock_spawn
bedrock_spawn:
        movl globalSp, %ebx
        movl %edi, 4+bedrock_heap(%ebx)
        movl bedrock_stack_size, %eax
        movl %eax, 8+bedrock_heap(%ebx)
        movl $return, %esi
        jmp scheduler_spawn

        .globl scheduler_exit
        .globl bedrock_exit
bedrock_exit:
        movl globalSp, %ebx
        movl bedrock_stack_size, %eax
        movl %eax, 4+bedrock_heap(%ebx)
        jmp scheduler_exit

afterContextSwitch:
        movl %edi, %eax
        subl $4, %ebx
        movl %ebx, globalSp
        movl bedrock_heap(%ebx), %esp
        POPA
        ret
        
        .globl scheduler_yield
        .globl bedrock_yield
bedrock_yield:
        PUSHA
        movl globalSp, %ebx
        movl %esp, bedrock_heap(%ebx)
        addl $4, %ebx
        movl $afterContextSwitch, %esi
        jmp scheduler_yield

        .globl scheduler_listen
        .globl bedrock_listen
bedrock_listen:
        PUSHA
        movl globalSp, %ebx
        movl %esp, bedrock_heap(%ebx)
        movl %edi, 8+bedrock_heap(%ebx)
        addl $4, %ebx
        movl $afterContextSwitch, %esi
        jmp scheduler_listen

        .globl scheduler_accept
        .globl bedrock_accept
bedrock_accept:
        PUSHA
        movl globalSp, %ebx
        movl %esp, bedrock_heap(%ebx)
        movl %edi, 8+bedrock_heap(%ebx)
        addl $4, %ebx
        movl $afterContextSwitch, %esi
        jmp scheduler_accept

        .globl scheduler_close
        .globl bedrock_close
bedrock_close:
        PUSHA
        movl globalSp, %ebx
        movl %esp, bedrock_heap(%ebx)
        movl %edi, 8+bedrock_heap(%ebx)
        addl $4, %ebx
        movl $afterContextSwitch, %esi
        jmp scheduler_close
        
        .globl scheduler_read
        .globl bedrock_read
bedrock_read:
        PUSHA
        movl globalSp, %ebx
        movl %esp, bedrock_heap(%ebx)
        movl %edi, 8+bedrock_heap(%ebx)
        subl $bedrock_heap, %esi
        movl %esi, 12+bedrock_heap(%ebx)
        movl %edx, 16+bedrock_heap(%ebx)
        addl $4, %ebx
        movl $afterContextSwitch, %esi
        jmp scheduler_read

        .globl scheduler_write
        .globl bedrock_write
bedrock_write:
        PUSHA
        movl globalSp, %ebx
        movl %esp, bedrock_heap(%ebx)
        movl %edi, 8+bedrock_heap(%ebx)
        subl $bedrock_heap, %esi
        movl %esi, 12+bedrock_heap(%ebx)
        movl %edx, 16+bedrock_heap(%ebx)
        addl $4, %ebx
        movl $afterContextSwitch, %esi
        jmp scheduler_write

        .globl scheduler_connect
        .globl bedrock_connect
bedrock_connect:
        PUSHA
        movl globalSp, %ebx
        movl %esp, bedrock_heap(%ebx)
        subl $bedrock_heap, %edi
        movl %edi, 8+bedrock_heap(%ebx)
        movl %esi, 12+bedrock_heap(%ebx)
        addl $4, %ebx
        movl $afterContextSwitch, %esi
        jmp scheduler_connect

        .globl scheduler_connected
        .globl bedrock_connected
bedrock_connected:
        PUSHA
        movl globalSp, %ebx
        movl %esp, bedrock_heap(%ebx)
        movl %edi, 8+bedrock_heap(%ebx)
        addl $4, %ebx
        movl $afterContextSwitch, %esi
        jmp scheduler_connected
