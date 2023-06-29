cmake_minimum_required(VERSION 3.15.0)

project(fortran_messagepack
    VERSION 0.2.0
    LANGUAGES Fortran)

add_subdirectory(src)
add_subdirectory(app/simple-demo)
add_subdirectory(test)
enable_testing()

# tests
add_test(NAME constructors
    COMMAND ./test/constructors)
add_test(NAME packing
    COMMAND ./test/packing)
add_test(NAME unpacking
    COMMAND ./test/unpacking)
add_test(NAME roundtrip
    COMMAND ./test/roundtrip)
