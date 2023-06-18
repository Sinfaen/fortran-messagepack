# constructor test
add_executable(constructors
    constructors.f90)
target_include_directories(constructors PUBLIC
    ${CMAKE_BINARY_DIR}/modules)
target_link_libraries(constructors messagepack)

# packing test
add_executable(packing
    packing.f90)
target_include_directories(packing PUBLIC
    ${CMAKE_BINARY_DIR}/modules)
target_link_libraries(packing messagepack)

# unpacking test
add_executable(unpacking
    unpacking.f90)
target_include_directories(unpacking PUBLIC
    ${CMAKE_BINARY_DIR}/modules)
target_link_libraries(unpacking messagepack)
