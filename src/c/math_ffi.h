#ifndef HASHPY_MATH_FFI_H
#define HASHPY_MATH_FFI_H

#include <stdint.h>
#include <inttypes.h>

#ifdef __cplusplus
extern "C" {
#endif

// Initialize the Haskell runtime
void math_ffi_init(void);

// Shutdown the Haskell runtime
void math_ffi_exit(void);

// Math functions
int32_t c_add(int32_t x, int32_t y);
int32_t c_multiply(int32_t x, int32_t y);

// Calculate the nth Fibonacci number (returns int64_t)
int64_t c_fib(int32_t n);

#ifdef __cplusplus
}
#endif

#endif // HASHPY_MATH_FFI_H
