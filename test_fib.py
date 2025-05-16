#!/usr/bin/env python3

import ctypes
import sys

# Load the library
lib_ext = "dylib" if sys.platform == "darwin" else "so"
lib_path = f"./libmathwrapper.{lib_ext}"
print(f"Loading library from: {lib_path}")

math_lib = ctypes.CDLL(lib_path)
print("Library loaded successfully")

# Initialize Haskell runtime
print("Initializing Haskell runtime...")
math_lib.math_ffi_init()

try:
    # Test Fibonacci with a small value first
    print("\nTesting Fibonacci with n=10...")
    math_lib.c_fib.argtypes = [ctypes.c_int32]
    math_lib.c_fib.restype = ctypes.c_char_p
    
    result = math_lib.c_fib(10)
    print(f"fib(10) = {result.decode('utf-8')}")
    
    # Free the string
    math_lib.free_fib_string.argtypes = [ctypes.c_void_p]
    math_lib.free_fib_string.restype = None
    math_lib.free_fib_string(result)
    
    print("Test completed successfully!")
    
finally:
    # Clean up
    print("\nCleaning up Haskell runtime...")
    math_lib.math_ffi_exit()
