#include <stdio.h>
#include <stdlib.h>
#include "src/c/math_ffi.h"

int main() {
    printf("Initializing Haskell runtime...\n");
    math_ffi_init();
    
    printf("Testing addition: 5 + 3 = %d\n", c_add(5, 3));
    printf("Testing multiplication: 5 * 3 = %d\n", c_multiply(5, 3));
    
    printf("Testing Fibonacci...\n");
    char* fib_str = c_fib(10);
    printf("fib(10) = %s\n", fib_str);
    free_fib_string(fib_str);
    
    printf("Cleaning up...\n");
    math_ffi_exit();
    return 0;
}
