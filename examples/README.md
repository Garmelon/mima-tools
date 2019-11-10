# Example MiMa programs

This folder contains a few example programs, both as `.mimasm` and as
assembled `.mima` files.

* [Basic programs](#basic-programs)
* [Advanced programs](#advanced-programs)

## Basic programs

### [`jmp_to_address_in_acc.mimasm`](jmp_to_address_in_acc.mimasm)

This program demonstrates two different techniques for jumping to an
address that is currently stored in the `ACC` register.

### [`call_ret.mimasm`](call_ret.mimasm)

This program demonstrates how the `CALL` and `RET` instructions
behave. It doesn't use any sort of stack, so the call depth is limited
to 1.

### [`call_ret_stack.mimasm`](call_ret_stack.mimasm)

This program works similar to `call_ret.mimasm`, but uses the `SP`
register for a stack. This way, it can have nested `CALL`s by storing
the content of the `RA` register on the stack.

## Advanced programs

### [`stack.mimasm`](stack.mimasm)

This program demonstrates the use of stack frames for calling a
function and passing parameters.  To call a function, it creates a
shared stack frame containing the function's input parameters and
enough space for its return values.

### [`fib.mimasm`](fib.mimasm)

This program calculates the first few fibonacci numbers and stores
them in consecutive memory locations. It uses a stack with stack
frames and recursive calls according to the following pattern:

``` c++
int fib(int n) {
    if (n == 0) return 0;
    if (n == 1) return 1;
    return fib(n - 1) + fib(n - 2);
}
```

This recursive solution for calculating fibonacci numbers is by far
not the most efficient, but it demonstrates recursion and stack usage
quite well.
