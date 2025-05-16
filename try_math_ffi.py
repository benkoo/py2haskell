#!/usr/bin/env python3
"""
Example script demonstrating how to use the Haskell math FFI from Python.
"""

import sys
import time
from ctypes import c_int32, CDLL, c_int64

# Determine the shared library extension
if sys.platform == "darwin":
    lib_ext = "dylib"
else:
    lib_ext = "so"

# Get the path to the shared library
lib_path = f"./libmathwrapper.{lib_ext}"

print(f"Loading library from: {lib_path}")

# Load the shared library
math_lib = CDLL(lib_path)
print("Library loaded successfully")

# Define function prototypes
math_lib.math_ffi_init.argtypes = []
math_lib.math_ffi_init.restype = None

math_lib.math_ffi_exit.argtypes = []
math_lib.math_ffi_exit.restype = None

# Arithmetic operations
math_lib.c_add.argtypes = [c_int32, c_int32]
math_lib.c_add.restype = c_int32

math_lib.c_subtract.argtypes = [c_int32, c_int32]
math_lib.c_subtract.restype = c_int32

math_lib.c_multiply.argtypes = [c_int32, c_int32]
math_lib.c_multiply.restype = c_int32

math_lib.c_divide.argtypes = [c_int32, c_int32]
math_lib.c_divide.restype = c_int32

# Define the Fibonacci function to return a 64-bit integer
math_lib.c_fib.argtypes = [c_int32]
math_lib.c_fib.restype = c_int64

def main():
    try:
        # Initialize the Haskell runtime
        print("Initializing Haskell runtime...")
        math_lib.math_ffi_init()
        
        # Test basic arithmetic operations
        x, y = 10, 3
        
        # Test addition
        print("\nTesting arithmetic operations:")
        result = math_lib.c_add(x, y)
        print(f"{x} + {y} = {result}")
        
        # Test subtraction
        result = math_lib.c_subtract(x, y)
        print(f"{x} - {y} = {result}")
        
        # Test multiplication
        result = math_lib.c_multiply(x, y)
        print(f"{x} * {y} = {result}")
        
        # Test division
        result = math_lib.c_divide(x, y)
        print(f"{x} / {y} = {result} (integer division)")
        
        # Test division by zero
        result = math_lib.c_divide(x, 0)
        print(f"{x} / 0 = {result} (handled division by zero)")
        
        # Test Fibonacci with timing
        print("\nTesting optimized Fibonacci implementation:")
        test_values = [10, 20, 30, 40, 50]  # Reduced the range for faster testing
        
        for n in test_values:
            print(f"\nCalculating fib({n})...")
            start_time = time.time()
            result = math_lib.c_fib(n)
            elapsed = time.time() - start_time
            print(f"fib({n}) = {result} (calculated in {elapsed:.6f} seconds)")
        
    finally:
        # Always clean up the Haskell runtime
        print("\nCleaning up Haskell runtime...")
        math_lib.math_ffi_exit()

if __name__ == "__main__":
    main()
