# unpack buffer demo
add_executable(unpack-buffer main.f90)
target_include_directories(unpack-buffer PUBLIC
    ${CMAKE_BINARY_DIR}/modules)
target_link_libraries(unpack-buffer messagepack)
