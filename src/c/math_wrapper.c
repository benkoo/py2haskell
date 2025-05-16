#include <HsFFI.h>
#include <stdint.h>
#include <stdlib.h>
#include <inttypes.h>
#include "math_ffi.h"

// Forward declarations from Haskell module
extern HsInt addFFI(HsInt x, HsInt y);
extern HsInt subtractFFI(HsInt x, HsInt y);
extern HsInt multiplyFFI(HsInt x, HsInt y);
extern HsInt divideFFI(HsInt x, HsInt y);
extern int64_t fibFFI(HsInt n);

// Initialize the Haskell runtime
void math_ffi_init(void) {
    static int is_haskell_initialized = 0;
    if (!is_haskell_initialized) {
        int argc = 2;
        char *argv[] = { "+RTS", "-A32m", NULL };
        char **pargv = argv;
        
        // Initialize Haskell runtime
        hs_init(&argc, &pargv);
        
        // Mark as initialized
        is_haskell_initialized = 1;
    }
}

// Shutdown the Haskell runtime
void math_ffi_exit(void) {
    // hs_exit() will clean up the runtime
    hs_exit();
}

// Wrapper functions
int32_t c_add(int32_t x, int32_t y) {
    return (int32_t)addFFI((HsInt)x, (HsInt)y);
}

int32_t c_subtract(int32_t x, int32_t y) {
    return (int32_t)subtractFFI((HsInt)x, (HsInt)y);
}

int32_t c_multiply(int32_t x, int32_t y) {
    return (int32_t)multiplyFFI((HsInt)x, (HsInt)y);
}

int32_t c_divide(int32_t x, int32_t y) {
    if (y == 0) return 0;  // Handle division by zero in C as well
    return (int32_t)divideFFI((HsInt)x, (HsInt)y);
}

// Calculate the nth Fibonacci number
int64_t c_fib(int32_t n) {
    return fibFFI((HsInt)n);
}
