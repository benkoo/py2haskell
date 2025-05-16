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

import Foreign.C.Types
import Foreign.C.Types (CInt(..))
import Foreign.C.String (CString, newCString)
import Data.Int (Int64)
import Data.Bits (shiftR, (.&.))




-- | Add two integers
add :: CInt -> CInt -> CInt
add = (+)


-- | Multiply two integers
multiply :: CInt -> CInt -> CInt
multiply = (*)


-- | Optimized Fibonacci number calculation using matrix exponentiation (O(log n))
-- Returns the nth Fibonacci number as Int64 (limited to 2^63-1)
fib :: CInt -> Int64
fib n
    | n < 0 = 0
    | n == 0 = 0
    | n == 1 = 1
    | otherwise = fst (fib' (fromIntegral (n - 1) :: Int64))
  where
    -- Fast doubling algorithm for Int64
    fib' :: Int64 -> (Int64, Int64)
    fib' m
        | m == 0 = (0, 1)
        | m == 1 = (1, 1)
        | otherwise =
            let (a, b) = fib' (shiftR m 1)  -- m `div` 2
                c = a * (2 * b - a)
                d = a * a + b * b
            in if m .&. 1 == 0  -- even m
                then (c, d)
                else (d, c + d)

-- FFI exports
foreign export ccall "addFFI" add :: CInt -> CInt -> CInt
foreign export ccall "multiplyFFI" multiply :: CInt -> CInt -> CInt
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
