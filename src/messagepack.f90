module messagepack

    ! implement buffer or c++ vector
    ! implement static buffer?
    ! implement packing
    ! implement unpacking
    ! implement file io
    use iso_fortran_env
    use messagepack_value
    use messagepack_pack
    use messagepack_unpack
    use byte_utilities
    implicit none
contains
    subroutine print_version()
        print *, "0.0.1"
    end subroutine

end module
