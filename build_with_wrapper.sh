#!/bin/bash
# Build script for MathFFI module with C wrapper

set -e  # Exit on error

# Configuration
LIB_NAME="mathwrapper"
BUILD_DIR="build"
SRC_DIR="src"
INCLUDE_DIR="include"
C_SRC_DIR="${SRC_DIR}/c"

# Create build directory if it doesn't exist
mkdir -p "${BUILD_DIR}"

# Get GHC paths
GHC_INCLUDE_DIR=$(ghc --print-libdir)/include
GHC_RTS_INCLUDE_DIR=$(ghc --print-libdir)/../rts/include
GHC_LIB_DIR=$(ghc --print-libdir)
GHC_RTS_LIB_DIR="${GHC_LIB_DIR}/aarch64-osx-ghc-$(ghc --numeric-version)"

# Set library extension based on OS
if [[ "$OSTYPE" == "darwin"* ]]; then
    LIB_EXT="dylib"
    LDFLAGS_SHARED="-dynamiclib"
    LDFLAGS_RPATH="-Wl,-rpath,${GHC_LIB_DIR} -Wl,-rpath,${GHC_RTS_LIB_DIR}"
else
    LIB_EXT="so"
    LDFLAGS_SHARED="-shared"
    LDFLAGS_RPATH="-Wl,-rpath=${GHC_LIB_DIR} -Wl,-rpath=${GHC_RTS_LIB_DIR}"
fi

# Output filenames
HASKELL_OBJ="${BUILD_DIR}/MathFFI.o"
C_OBJ="${BUILD_DIR}/math_wrapper.o"
OUTPUT_LIB="${BUILD_DIR}/lib${LIB_NAME}.${LIB_EXT}"

# Compile Haskell module
echo "Compiling Haskell module..."
ghc -dynamic -c -O2 \
    -o "${HASKELL_OBJ}" \
    -stubdir "${BUILD_DIR}" \
    -odir "${BUILD_DIR}" \
    -hidir "${BUILD_DIR}" \
    "${SRC_DIR}/MathFFI.hs"

# Compile C wrapper
echo "Compiling C wrapper..."
GHC_LIB_DIR=$(ghc --print-libdir)
GHC_VERSION=$(ghc --numeric-version)
GHC_FFI_INCLUDE="${GHC_LIB_DIR}/aarch64-osx-ghc-${GHC_VERSION}/rts-1.0.2/include"
GHC_RTS_INCLUDE="${GHC_LIB_DIR}/include"

# Check if the files exist
if [ ! -f "${GHC_FFI_INCLUDE}/HsFFI.h" ]; then
    echo "Error: Could not find HsFFI.h in ${GHC_FFI_INCLUDE}"
    echo "Trying to find HsFFI.h in other locations..."
    # Try to find HsFFI.h in common locations
    FOUND_HSFFI=$(find "${GHC_LIB_DIR}" -name "HsFFI.h" 2>/dev/null | head -n 1)
    if [ -n "${FOUND_HSFFI}" ]; then
        GHC_FFI_INCLUDE=$(dirname "${FOUND_HSFFI}")
        echo "Found HsFFI.h in ${GHC_FFI_INCLUDE}"
    else
        echo "Error: Could not find HsFFI.h in any standard location"
        exit 1
    fi
fi

echo "Using GHC FFI include: ${GHC_FFI_INCLUDE}"
echo "Using GHC RTS include: ${GHC_RTS_INCLUDE}"

cc -c -fPIC \
    -I"${GHC_FFI_INCLUDE}" \
    -I"${GHC_RTS_INCLUDE}" \
    -I"${SRC_DIR}/c" \
    -o "${C_OBJ}" \
    "${C_SRC_DIR}/math_wrapper.c"

# Link everything together
echo "Linking final library..."
cc ${LDFLAGS_SHARED} \
    -o "${OUTPUT_LIB}" \
    "${C_OBJ}" \
    "${HASKELL_OBJ}" \
    "${GHC_RTS_LIB_DIR}/libHSrts-ghc$(ghc --numeric-version).${LIB_EXT}" \
    "${GHC_RTS_LIB_DIR}/libHSbase-4.18.3.0-ghc$(ghc --numeric-version).${LIB_EXT}" \
    "${GHC_RTS_LIB_DIR}/libHSghc-bignum-1.3-ghc$(ghc --numeric-version).${LIB_EXT}" \
    "${GHC_RTS_LIB_DIR}/libHSghc-prim-0.10.0-ghc$(ghc --numeric-version).${LIB_EXT}" \
    -L"${GHC_LIB_DIR}" \
    -L"${GHC_RTS_LIB_DIR}" \
    -lffi \
    -liconv \
    ${LDFLAGS_RPATH} \
    -Wl,-install_name,@rpath/lib${LIB_NAME}.${LIB_EXT}

# Copy the library to the root directory for easier access
cp "${OUTPUT_LIB}" "./lib${LIB_NAME}.${LIB_EXT}"

echo "Build completed successfully!"
echo "Library built: ${OUTPUT_LIB}"
echo "Copied to: ./lib${LIB_NAME}.${LIB_EXT}"

# Show library dependencies
if command -v otool &> /dev/null; then
    echo -e "\nLibrary dependencies:"
    otool -L "${OUTPUT_LIB}" || true
elif command -v ldd &> /dev/null; then
    echo -e "\nLibrary dependencies:"
    ldd "${OUTPUT_LIB}" || true
fi
