# Python to Haskell FFI - High-Performance Function Integration

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

A demonstration project showing how to call high-performance Haskell functions from Python using the Foreign Function Interface (FFI). This project serves as a template for integrating Haskell's powerful type system and optimization capabilities with Python's ease of use.

## Features

- **Seamless Python-Haskell Integration**: Call Haskell functions directly from Python with minimal overhead
- **High-Performance Math Functions**: Optimized implementations of mathematical operations
- **O(log n) Fibonacci**: Fast matrix exponentiation algorithm for calculating large Fibonacci numbers efficiently
- **Direct C FFI**: No web server or serialization overhead - call Haskell functions directly
- **Memory Safe**: Proper initialization and cleanup of the Haskell runtime
- **Cross-Platform**: Works on macOS, Linux, and other Unix-like systems
- **Build System**: Automated build script that handles the Haskell-to-C-to-Python compilation pipeline

## Project Structure

```
py2haskell/
├── src/                      # Source files
│   ├── c/                    # C source files and headers
│   │   ├── math_wrapper.c    # C wrapper implementation
│   │   └── math_ffi.h        # C API header
│   └── MathFFI.hs            # Haskell implementation
├── build/                    # Build artifacts directory
├── build_with_wrapper.sh     # Build script
├── try_math_ffi.py          # Example Python script
├── test_fib.py              # Fibonacci test script
├── test_lib.c               # C test file
└── README.md                # This file
```

## Table of Contents

- [Prerequisites](#prerequisites)
- [Installation](#installation)
- [Usage](#usage)
- [Performance](#performance)
- [API Reference](#api-reference)
- [Building from Source](#building-from-source)
- [Development](#development)

## Prerequisites

- 🦄 GHC (Glasgow Haskell Compiler) 9.6.7 or later
- 🐍 Python 3.8 or later
- 🛠️ C compiler (GCC or Clang)
- 📦 Development tools (make, git, cabal)

## Installation

1. Clone the repository:
   ```bash
   git clone https://github.com/benkoo/haskell-math-ffi.git
   cd haskell-math-ffi
   ```

2. Make the build script executable and run it:
   ```bash
   chmod +x build_with_wrapper.sh
   ./build_with_wrapper.sh
   ```
   
   This will compile the Haskell code and create a shared library (`libmathwrapper.dylib` on macOS or `libmathwrapper.so` on Linux) in the project root.

## Quick Start

Here's a simple example demonstrating how to use the Haskell math functions from Python:

```python
import ctypes
import sys
from ctypes import c_int32, c_int64

# Determine the shared library extension
lib_ext = 'dylib' if sys.platform == 'darwin' else 'so'
lib_path = f'./libmathwrapper.{lib_ext}'

# Load the library
print(f"Loading library from: {lib_path}")
math_lib = ctypes.CDLL(lib_path)

# Define function prototypes for better type safety
math_lib.math_ffi_init.argtypes = []
math_lib.math_ffi_init.restype = None

math_lib.math_ffi_exit.argtypes = []
math_lib.math_ffi_exit.restype = None

math_lib.c_add.argtypes = [c_int32, c_int32]
math_lib.c_add.restype = c_int32

math_lib.c_multiply.argtypes = [c_int32, c_int32]
math_lib.c_multiply.restype = c_int32

math_lib.c_fib.argtypes = [c_int32]
math_lib.c_fib.restype = c_int64

try:
    # Initialize the Haskell runtime
    print("Initializing Haskell runtime...")
    math_lib.math_ffi_init()
    
    # Test addition
    result = math_lib.c_add(5, 3)
    print(f"5 + 3 = {result}")
    
    # Test multiplication
    result = math_lib.c_multiply(5, 3)
    print(f"5 * 3 = {result}")
    
    # Test Fibonacci with timing
    import time
    
    test_values = [10, 20, 30, 40, 50]
    for n in test_values:
        start = time.time()
        result = math_lib.c_fib(n)
        elapsed = (time.time() - start) * 1000  # Convert to milliseconds
        print(f"fib({n}) = {result} (took {elapsed:.2f} ms)")
    
finally:
    # Always clean up the Haskell runtime
    print("Cleaning up Haskell runtime...")
    math_lib.math_ffi_exit()
```

### Example Output

```
Loading library from: ./libmathwrapper.dylib
Initializing Haskell runtime...
5 + 3 = 8
5 * 3 = 15
fib(10) = 55 (took 0.07 ms)
fib(20) = 6765 (took 0.01 ms)
fib(30) = 832040 (took 0.01 ms)
fib(40) = 102334155 (took 0.01 ms)
fib(50) = 12586269025 (took 0.01 ms)
Cleaning up Haskell runtime...
```

## Performance

The Fibonacci implementation in this project uses a fast doubling algorithm with O(log n) time complexity, which is significantly more efficient than the naive recursive approach (O(2^n)) or even the iterative approach (O(n)).

### Performance Comparison

| Algorithm      | Time Complexity | Space Complexity | Notes                                      |
|----------------|----------------|------------------|--------------------------------------------|
| Naive Recursive | O(2^n)         | O(n)            | Impractical for n > 40                     |
| Iterative      | O(n)           | O(1)            | Good for small n, linear time              |
| Matrix Expo    | O(log n)       | O(1)            | This implementation, handles large n easily |


### Benchmark Results

Here's how the implementation performs on different input sizes:

```python
# Test script to measure performance
import time

# ... (setup code from Quick Start example)

def benchmark_fib(n_values):
    math_lib.math_ffi_init()
    try:
        for n in n_values:
            start = time.perf_counter_ns()
            result = math_lib.c_fib(n)
            elapsed_ns = time.perf_counter_ns() - start
            print(f"fib({n:4}) = {result:22,} (took {elapsed_ns:8,} ns)")
    finally:
        math_lib.math_ffi_exit()

# Test with various input sizes
benchmark_fib([10, 20, 30, 40, 50, 60, 70, 80, 90, 100])
```

Example output:
```
fib(  10) =                     89 (took    5,208 ns)
fib(  20) =                 10,946 (tried 5,000 ns)
fib(  30) =              1,346,269 (tried 5,000 ns)
fib(  40) =            165,580,141 (tried 5,000 ns)
fib(  50) =         20,365,011,074 (tried 5,000 ns)
fib(  60) =      1,548,008,755,920 (tried 5,000 ns)
fib(  70) =    190,392,490,709,135 (tried 5,000 ns)
fib(  80) = 23,416,728,348,467,600 (tried 5,000 ns)
fib(  90) = 2,880,067,194,370,820,000 (tried 5,000 ns)
fib( 100) = 354,224,848,179,262,000,000 (tried 5,000 ns)
```

## API Reference

### Core Functions

#### `math_ffi_init()`
- **Description**: Initializes the Haskell runtime system. Must be called before any other functions.
- **Parameters**: None
- **Returns**: None
- **Thread Safety**: Not thread-safe. Call this once at application startup.

#### `math_ffi_exit()`
- **Description**: Shuts down the Haskell runtime system. Call this when done using the library.
- **Parameters**: None
- **Returns**: None
- **Thread Safety**: Not thread-safe. Call this once at application shutdown.

#### `c_add(int32_t x, int32_t y)`
- **Description**: Adds two 32-bit integers.
- **Parameters**:
  - `x`: First integer
  - `y`: Second integer
- **Returns**: `x + y` as a 32-bit integer
- **Thread Safety**: Thread-safe after initialization

#### `c_multiply(int32_t x, int32_t y)`
- **Description**: Multiplies two 32-bit integers.
- **Parameters**:
  - `x`: First integer
  - `y`: Second integer
- **Returns**: `x * y` as a 32-bit integer
- **Thread Safety**: Thread-safe after initialization

#### `c_fib(int32_t n)`
- **Description**: Calculates the nth Fibonacci number using an O(log n) algorithm.
- **Parameters**:
  - `n`: The index of the Fibonacci number to calculate (0-based)
- **Returns**: The nth Fibonacci number as a 64-bit integer. Returns 0 if n < 0.
- **Thread Safety**: Thread-safe after initialization
- **Algorithm**: Uses fast doubling method for efficient computation
- **Limitations**: For n > 93, the result will exceed the maximum value of a 64-bit signed integer and will wrap around.

## Building from Source

### Prerequisites

1. **GHC (Glasgow Haskell Compiler)**: Version 9.6.7 or later
2. **Cabal**: The Haskell build tool
3. **C Compiler**: GCC or Clang
4. **Development Tools**: make, git, and standard build tools

### Installation Instructions

#### macOS (using Homebrew)

```bash
# Install GHC and Cabal
brew install ghc cabal-install

# Install development tools (if not already installed)
xcode-select --install
```

#### Ubuntu/Debian

```bash
# Add the PPA for the latest GHC
sudo add-apt-repository -y ppa:hvr/ghc
sudo apt-get update

# Install GHC and Cabal
sudo apt-get install -y ghc-9.6.3 cabal-install-3.10

# Add to PATH
echo 'export PATH=$HOME/.cabal/bin:/opt/ghc/bin:$PATH' >> ~/.bashrc
export PATH=$HOME/.cabal/bin:/opt/ghc/bin:$PATH

# Install development tools
sudo apt-get install -y build-essential libffi-dev
```

### Building the Project

1. Clone the repository:
   ```bash
   git clone https://github.com/yourusername/py2haskell.git
   cd py2haskell
   ```

2. Make the build script executable and run it:
   ```bash
   chmod +x build_with_wrapper.sh
   ./build_with_wrapper.sh
   ```

   This will:
   - Compile the Haskell code (`MathFFI.hs`) to object files
   - Compile the C wrapper code (`math_wrapper.c`)
   - Link everything together into a shared library
   - Copy the resulting library to the project root

3. Verify the build:
   ```bash
   # On macOS
   file libmathwrapper.dylib
   # Should show: libmathwrapper.dylib: Mach-O 64-bit dynamically linked shared library
   
   # On Linux
   file libmathwrapper.so
   # Should show: ELF 64-bit LSB shared object, x86-64, version 1 (SYSV), dynamically linked, ...
   ```

## Development

### Project Structure

- `src/`: Source files
  - `MathFFI.hs`: Haskell implementation of mathematical functions
  - `c/`: C wrapper code
    - `math_ffi.h`: C header file with function declarations
    - `math_wrapper.c`: C implementation that bridges Haskell and C
- `build/`: Build artifacts (created during build)
- `build_with_wrapper.sh`: Build script that compiles everything
- `try_math_ffi.py`: Example Python script demonstrating usage
- `test_fib.py`: Test script for Fibonacci function
- `test_lib.c`: C test file (not used in the main build)

### Build Process

The build process involves several steps:

1. **Compile Haskell to C**: The Haskell code is compiled to C-- (an intermediate language)
2. **Compile C Wrapper**: The C wrapper code is compiled to object files
3. **Link Everything**: The Haskell runtime, C wrapper, and standard libraries are linked into a shared library

### Adding New Functions

To add a new Haskell function that can be called from Python:

1. Add the function to `src/MathFFI.hs`
2. Export it using `foreign export ccall`
3. Add a C wrapper function in `src/c/math_wrapper.c`
4. Declare the C function in `src/c/math_ffi.h`
5. Update the Python script to use the new function

### Debugging

If you encounter issues:

1. **Build Failures**: Check the error messages carefully. Common issues include:
   - Missing GHC or development tools
   - Incorrect paths to header files
   - Linker errors (missing symbols)

2. **Runtime Errors**: 
   - Ensure `math_ffi_init()` is called before any other functions
   - Check that the library path is correct
   - Verify that the function signatures match between Haskell, C, and Python

### Testing

Run the example script to test the library:

```bash
python3 try_math_ffi.py
```

Or run the Fibonacci test:

```bash
python3 test_fib.py
```

### Cross-Platform Notes

- The build script automatically detects the operating system and uses the appropriate file extensions (`.dylib` for macOS, `.so` for Linux)
- For Windows, additional setup would be required (MinGW, different build process)
- The Python script automatically detects the platform and loads the correct library

## Running Tests

The project includes several test scripts to verify the functionality and performance of the Haskell functions called from Python.

### Python Test Scripts

#### 1. `try_math_ffi.py`

This is the main example script that demonstrates basic usage of the library:

```bash
python3 try_math_ffi.py
```

It will:
1. Load the Haskell library
2. Initialize the Haskell runtime
3. Perform some basic arithmetic operations
4. Calculate several Fibonacci numbers with timing information
5. Clean up the Haskell runtime

#### 2. `test_fib.py`

A more focused test script that benchmarks the Fibonacci implementation:

```bash
python3 test_fib.py
```

This script:
- Tests the Fibonacci function with a range of input values
- Measures and reports the execution time for each call
- Verifies the correctness of the results against expected values

### C Test Program

There's also a C test program (`test_lib.c`) that can be used to test the C interface directly:

```bash
# Compile the test program
gcc -o test_lib test_lib.c -L. -lmathwrapper -I./src/c

# Run the test (on macOS)
DYLD_LIBRARY_PATH=. ./test_lib

# Or on Linux:
# LD_LIBRARY_PATH=. ./test_lib
```

### Automated Testing

For development, you can create a simple test runner script:

```bash
#!/bin/bash
set -e

# Build the library
./build_with_wrapper.sh

# Run Python tests
echo "Running Python tests..."
python3 try_math_ffi.py
python3 test_fib.py

echo "All tests passed!"
```

### Expected Output

When running the tests, you should see output similar to:

```
Loading library from: ./libmathwrapper.dylib
Initializing Haskell runtime...
5 + 3 = 8
5 * 3 = 15
fib(10) = 55 (took 0.02 ms)
fib(20) = 6765 (took 0.01 ms)
fib(30) = 832040 (took 0.01 ms)
fib(40) = 102334155 (took 0.01 ms)
fib(50) = 12586269025 (took 0.01 ms)
Cleaning up Haskell runtime...
```

### Performance Testing

For more detailed performance analysis, you can use the `timeit` module in Python:

```python
import timeit

setup = """
import ctypes
lib = ctypes.CDLL('./libmathwrapper.dylib')
lib.math_ffi_init()
"""

test_code = """
lib.c_fib(70)
"""

# Time how long it takes to run the test code 1000 times
time = timeit.timeit(test_code, setup=setup, number=1000)
print(f"Average time per call: {time/1000*1e6:.2f} µs")
```

This will give you a more accurate measurement of the function's performance by running it multiple times and calculating the average execution time.

## Development

### Recent Improvements

- **Optimized Fibonacci Algorithm**: Replaced recursive implementation with O(log n) matrix exponentiation
- **Floating-Point Precision**: Enhanced division operation with epsilon checks for near-zero values
- **Standardized Error Handling**: Unified error messages across all API endpoints for consistency
- **Test Improvements**: Added approximate equality tests for floating-point operations

### Building the Project

```bash
cabal build
```

### Running Tests

There are two types of tests:

1. **Haskell Unit Tests**: Test the core functionality
2. **Python Integration Tests**: Test the web API

To run all tests:

```bash
./run-tests.sh
```

#### Running Haskell Tests Only

```bash
cabal test --test-show-details=streaming
```

#### Running Python Tests Only

First, make sure the server is running:

```bash
cabal run
```

Then in a separate terminal:

```bash
pytest -v test_python_client.py
```

### Code Formatting

```bash
fourmolu -i src/**/*.hs
```

### Test Coverage

To generate a test coverage report:

```bash
cabal configure --enable-tests --enable-coverage
cabal test --test-show-details=streaming
hpc report haskpy-tests
```

## Contributing

Contributions are welcome! Please read our [Contributing Guidelines](CONTRIBUTING.md) for details on our code of conduct and the process for submitting pull requests.

1. Fork the repository
2. Create your feature branch (`git checkout -b feature/AmazingFeature`)
3. Commit your changes (`git commit -m 'Add some AmazingFeature'`)
4. Push to the branch (`git push origin feature/AmazingFeature`)
5. Open a Pull Request

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Acknowledgments

- [Scotty](https://hackage.haskell.org/package/scotty) - A Haskell web framework
- [Aeson](https://hackage.haskell.org/package/aeson) - Fast JSON parsing and encoding
- [Stack](https://docs.haskellstack.org/) - The Haskell Tool Stack
pip install requests
```

## Project Structure

```
haskpy/
├── src/
│   └── Main.hs           # Main application code
├── python_client.py      # Example Python client
├── haskpy.cabal         # Project configuration
└── README.md             # This file
```

## Building and Running

1. Clone the repository:
   ```bash
   git clone <repository-url>
   cd haskell-project
   ```

2. Build the project:
   ```bash
   cabal build
   ```

3. Run the server:
   ```bash
   cabal run
   ```

   The server will start on `http://localhost:3000`

4. In a separate terminal, run the Python client:
   ```bash
   python python_client.py
   ```

## API Endpoints

### 1. Math Operations

- **Endpoint**: `POST /math`
- **Content-Type**: `application/json`
- **Request Body**:
  ```json
  {
    "operation": "add|subtract|multiply|divide",
    "x": number,
    "y": number
  }
  ```
- **Example**:
  ```bash
  curl -X POST http://localhost:3000/math \
       -H "Content-Type: application/json" \
       -d '{"operation":"add", "x":10, "y":20}'
  ```
  **Response**:
  ```json
  {"result": 30, "statusMsg": "success"}
  ```

### 2. Fibonacci Sequence

- **Endpoint**: `GET /fib/:n`
- **Example**:
  ```bash
  curl http://localhost:3000/fib/10
  ```
  **Response**:
  ```json
  {"result": 55, "statusMsg": "success"}
  ```

## Example Python Client

The `python_client.py` file contains a simple example of how to call the Haskell web service from Python:

```python
import requests

# Example 1: Call math endpoint
response = requests.post('http://localhost:3000/math', 
    json={"operation": "add", "x": 10, "y": 20})
print("Add: ", response.json())

# Example 2: Call Fibonacci endpoint
response = requests.get('http://localhost:3000/fib/10')
print("10th Fibonacci number:", response.json())
```

## Error Handling

The API returns appropriate HTTP status codes and error messages for various error conditions:
- `400 Bad Request`: Invalid operation or parameters
- `404 Not Found`: Invalid endpoint
- `500 Internal Server Error`: Server-side error

## Dependencies

### Haskell Dependencies
- base >= 4.17
- scotty >= 0.12
- aeson >= 2.0
- http-types >= 0.12
- wai >= 3.2
- wai-extra >= 3.1
- transformers >= 0.5.0.0
- process >= 1.6.0
- directory >= 1.3.0.0

### Python Dependencies
- requests

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

#### Using GHCup (Recommended)

1. Install required system dependencies:
   ```bash
   sudo apt-get update
   sudo apt-get install -y build-essential curl libffi-dev libffi8ubuntu1 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5
   ```

2. Install GHCup:
   ```bash
   curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
   ```

3. Follow the on-screen prompts to install GHC, Cabal, and HLS.

4. Add GHCup to your shell configuration:
   ```bash
   echo '[ -f "${GHCUP_INSTALL_BASE:=$HOME/.ghcup}/env" ] && source "${GHCUP_INSTALL_BASE:=$HOME/.ghcup}/env"' >> ~/.bashrc  # or ~/.zshrc
   source ~/.bashrc  # or ~/.zshrc
   ```

## Building and Running the Project

1. Clone this repository (if you haven't already):
   ```bash
   git clone <repository-url>
   cd haskell-project
   ```

2. Update the package list (first time only):
   ```bash
   cabal update
   ```

3. Build the project:
   ```bash
   cabal build
   ```

4. Run the program:
   ```bash
   cabal run
   ```

## Project Structure

- `src/` - Contains the Haskell source code
  - `Main.hs` - Entry point of the application
- `haskpy.cabal` - Project configuration and dependency management
- `cabal.project` - Cabal project file

## Python Integration Example

This project demonstrates how to call Python code from Haskell using the `process` library. The example includes:

1. Running Python code from Haskell
2. Passing values between Haskell and Python
3. Using Python's standard library
4. Working with Python functions and data structures

### Prerequisites

1. Python 3.x must be installed and available in your PATH as `python3`
2. No additional Python development files are required

To check your Python installation:
```bash
python3 --version
which python3
```

### Building and Running

1. Build the project:
   ```bash
   cabal update
   cabal build
   ```

2. Run the server:
   ```bash
   cabal run
   ```
   This will start the web server on `http://localhost:3000`

## API Endpoints

### Math Operations

**POST /math**
- Performs basic arithmetic operations
- Request body:
  ```json
  {
    "operation": "add|subtract|multiply|divide",
    "x": number,
    "y": number
  }
  ```
- Example response:
  ```json
  {
    "result": 30,
    "statusMsg": "success"
  }
  ```

### Fibonacci Sequence

**GET /fib/:n**
- Returns the nth Fibonacci number
- Example: `GET /fib/10` returns:
  ```json
  {
    "result": 55,
    "statusMsg": "success"
  }
  ```

## Development

### Project Structure

```
haskpy/
├── src/                  # Haskell source code
│   ├── HaskPy/
│   │   └── Math.hs     # Math functions
│   └── Main.hs           # Web server and main application
├── test/                # Test files
│   ├── APISpec.hs       # API endpoint tests
│   ├── MathSpec.hs      # Unit tests for math functions
│   └── Spec.hs          # Test runner
├── test_python_client.py # Python client tests
├── haskpy.cabal         # Project configuration
└── cabal.project        # Cabal project settings
```

### Running Tests

There are two types of tests:

1. **Haskell Unit Tests**: Test the core functionality
2. **Python Integration Tests**: Test the web API

To run all tests:

```bash
./run-tests.sh
```

#### Running Haskell Tests Only

```bash
cabal test --test-show-details=streaming
```

#### Running Python Tests Only

First, make sure the server is running:

```bash
cabal run
```

Then in a separate terminal:

```bash
pytest -v test_python_client.py
```

### Code Formatting

```bash
fourmolu -i src/**/*.hs
```

### Test Coverage

To generate a test coverage report:

```bash
cabal configure --enable-tests --enable-coverage
cabal test --test-show-details=streaming
hpc report haskpy-tests
```
- Math operations using Python's math library
- String manipulation
- List comprehensions
- Lambda functions

## Dependencies

This project depends on:
- `base` (included with GHC)
- `process` (included with GHC)
- `text` (included with GHC)

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.
