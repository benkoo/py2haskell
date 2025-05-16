{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CApiFFI #-}

-- |
-- MathFFI - A module providing high-performance mathematical functions
-- with a C-compatible foreign function interface.
--
-- This module implements optimized mathematical operations that can be called
-- from C and other languages through the FFI.
--
module MathFFI where

import Foreign.C.Types (CInt(..))
import Data.Int (Int64)




-- | Add two integers
add :: CInt -> CInt -> CInt
add = (+)


-- | Subtract the second integer from the first
subtract' :: CInt -> CInt -> CInt
subtract' = (-)


-- | Multiply two integers
multiply :: CInt -> CInt -> CInt
multiply = (*)


-- | Divide two integers. Returns 0 if dividing by zero.
divide :: CInt -> CInt -> CInt
divide _ 0 = 0  -- Handle division by zero
divide x y = x `div` y

-- | Fibonacci number calculation
-- Returns the nth Fibonacci number as Int64 (limited to 2^63-1)
-- Fibonacci sequence: 0, 1, 1, 2, 3, 5, 8, 13, ...
fib :: CInt -> Int64
fib n
    | n < 0 = 0
    | otherwise = fib' (fromIntegral n) 0 1
  where
    fib' :: Int64 -> Int64 -> Int64 -> Int64
    fib' 0 a _ = a
    fib' n a b = fib' (n - 1) b (a + b)

-- FFI exports
foreign export ccall "addFFI" add :: CInt -> CInt -> CInt
foreign export ccall "subtractFFI" subtract' :: CInt -> CInt -> CInt
foreign export ccall "multiplyFFI" multiply :: CInt -> CInt -> CInt
foreign export ccall "divideFFI" divide :: CInt -> CInt -> CInt
foreign export ccall "fibFFI" fib :: CInt -> Int64

-- | Initialize the Haskell runtime.
-- This should be called once before any other FFI functions.
foreign export ccall "hs_init_math" hsInitMath :: IO ()
hsInitMath :: IO ()
hsInitMath = return ()

-- | Shutdown the Haskell runtime.
-- This should be called when the library is no longer needed.
foreign export ccall "hs_exit_math" hsExitMath :: IO ()
hsExitMath :: IO ()
hsExitMath = return ()
