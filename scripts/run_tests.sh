#!/bin/sh
# Script to run assembly tests

set -e # Exit immediately if a command exits with a non-zero status

# Get architecture
ARCH=$(uname -m)
echo "Running on architecture: $ARCH"

# List all assembly files
ASM_FILES=$(ls *.s 2>/dev/null || echo "No assembly files found")
echo "Found assembly files: $ASM_FILES"

# Function to assemble and run tests
run_test() {
    file=$1
    basename=${file%.s}
    
    echo "===== Testing $file ====="
    
    # Assemble the file
    as -o "${basename}.o" "$file"
    
    # Link the object file
    gcc -o "${basename}" "${basename}.o"
    
    # Run the executable
    echo "Running ${basename}..."
    ./"${basename}"
    
    # Check exit status
    if [ $? -eq 0 ]; then
        echo "✅ Test passed!"
    else
        echo "❌ Test failed!"
        exit 1
    fi
}

# Run tests for each assembly file
if [ -z "$ASM_FILES" ] || [ "$ASM_FILES" = "No assembly files found" ]; then
    echo "No assembly files to test"
    exit 1
fi

for file in $ASM_FILES; do
    run_test "$file"
done

echo "All tests completed successfully!"