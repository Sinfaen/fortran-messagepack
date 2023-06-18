# simple demo
add_executable(simple-demo main.f90)
target_include_directories(simple-demo PUBLIC
    ${CMAKE_BINARY_DIR}/modules)
target_link_libraries(simple-demo messagepack)