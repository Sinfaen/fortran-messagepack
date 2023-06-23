module messagepack

    ! implement buffer or c++ vector
    ! implement static buffer?
    ! implement packing
    ! implement unpacking
    ! implement file io
    use iso_fortran_env
    use messagepack_value
    use messagepack_pack
    use messagepack_user
    use messagepack_unpack
    use byte_utilities

    implicit none
contains
    subroutine print_version()
        print *, "0.1.3"
    end subroutine

    subroutine print_bytes_as_hex(bytes, addhexmark)
        ! prints a buffer of bytes as the unsigned hex version
        ! @param[in] bytes - byte buffer to print
        ! @param[in] addhexmark - If true, print with 0x prepended
        ! @returns none
        byte, dimension(:), allocatable, intent(in) :: bytes
        logical, intent(in) :: addhexmark

        integer :: i
        integer :: val
        write(*, "(A2)", advance="no") "[ "
        if (addhexmark) then
            do i = 1,size(bytes)
                val = int8_as_unsigned(bytes(i))
                write(*, '("0x", Z2.2, " ")', advance="no") val
            end do
        else
            do i = 1,size(bytes)
                val = int8_as_unsigned(bytes(i))
                write(*, '(Z2.2, " ")', advance="no") val
            end do
        end if
        write(*,*) "]"
    end subroutine

end module
