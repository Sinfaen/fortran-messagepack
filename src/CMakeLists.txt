# build the fortran-messagepack library
add_library(messagepack SHARED
    messagepack.f90
    messagepack_value.f90
    messagepack_user.f90
    messagepack_pack.f90
    messagepack_unpack.f90
    byte_utilities.f90)
set_target_properties(messagepack PROPERTIES
    Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/modules)
