#!/bin/bash

quiet=false

# Parse options
while getopts "q" opt; do
  case $opt in
    q)
      quiet=true
      ;;
    *)
      ;;
  esac
done

shift $((OPTIND - 1))

# Check if CMakeLists.txt is present
if [[ ! -f CMakeLists.txt ]]; then
    echo "!!! No CMakeLists.txt in $(pwd). Exiting..."
    exit
fi

# Run cmake with the specified options
if [ "$quiet" = false ]; then
    cmake -S . -B build \
        -DCMAKE_EXPORT_COMPILE_COMMANDS=1 \
        -DCMAKE_C_COMPILER=clang \
        -DCMAKE_CXX_COMPILER=clang++ \
        -DFB_STDLIB='libc++' \
        -G "Unix Makefiles"
else
    cmake -S . -B build \
        -DCMAKE_EXPORT_COMPILE_COMMANDS=1 \
        -DCMAKE_C_COMPILER=clang \
        -DCMAKE_CXX_COMPILER=clang++ \
        -DFB_STDLIB='libc++' \
        -G "Unix Makefiles" &> /dev/null
fi

# Check if cmake configuration was successful
if [[ $? -ne 0 ]]; then 
    printf "\n!!! CMake configuration failed. Exiting...\n"
    exit
fi

# Run make
if [ "$quiet" = false ]; then
    make -C build
else
    make -C build &> /dev/null
fi
if [[ $? -ne 0 ]]; then
    printf "\n!!! Compilation failed. Exiting...\n"
    exit
fi

# Iterate over build files and run executable ones
for f in $(ls build/*); do
    if [[ -x $f ]]; then
        if [ "$quiet" = false ]; then
            printf "\n----- Running \'$f\' ------\n"
        fi
        ./$f
    fi
done
