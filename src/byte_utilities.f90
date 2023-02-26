module byte_utilities
    use iso_fortran_env
    use,intrinsic :: ieee_arithmetic
    implicit none
    public
    contains
        logical function detect_little_endian()
            ! used by the library to detect host endianness
            ! Note: DOES NOT HANDLE MIDDLE-ENDIAN
            ! @returns .true. if little endian, .false. otherwise
            detect_little_endian = (1 == transfer([1_int8, 0_int8], 0_int16) )
        end function

        subroutine print_endianness()
            ! debugging function to print out whether the library
            ! thinks the host system is little or big endian
            if (detect_little_endian()) then
                print *, "Detected System Endianness: Little"
            else
                print *, "Detected System Endiannes: Big"
            end if
        end subroutine

        integer(kind=int16) function bytes_be_int_le_2(bytes)
            ! converts bytes in big-endian to an int16 in little endian
            byte, dimension(2), intent(in) :: bytes
            bytes_be_int_le_2 = transfer([bytes(2), bytes(1)], 0_int16)
        end function

        integer(kind=int32) function bytes_be_int_le_4(bytes)
        ! converts bytes in big-endian to an int32 in little endian
            byte, dimension(4), intent(in) :: bytes
            bytes_be_int_le_4 = transfer([bytes(4), bytes(3), bytes(2), bytes(1)], 0_int32)
        end function

        integer(kind=int64) function bytes_be_int_le_8(bytes)
        ! converts bytes in big-endian to an int64 in little endian
            byte, dimension(8), intent(in) :: bytes
            bytes_be_int_le_8 = transfer([bytes(8), bytes(7), bytes(6), bytes(5), &
                bytes(4), bytes(3), bytes(2), bytes(1)], 0_int64)
        end function

        integer(kind=int16) function bytes_be_int_be_2(bytes)
            ! converts bytes in big-endian to an int16 in big endian
            byte, dimension(2), intent(in) :: bytes
            bytes_be_int_be_2 = transfer([bytes(1), bytes(2)], 0_int16)
        end function

        integer(kind=int32) function bytes_be_int_be_4(bytes)
        ! converts bytes in big-endian to an int32 in little endian
            byte, dimension(4), intent(in) :: bytes
            bytes_be_int_be_4 = transfer([bytes(1), bytes(2), bytes(3), bytes(4)], 0_int32)
        end function

        integer(kind=int64) function bytes_be_int_be_8(bytes)
        ! converts bytes in big-endian to an int64 in little endian
            byte, dimension(8), intent(in) :: bytes
            bytes_be_int_be_8 = transfer([bytes(1), bytes(2), bytes(3), bytes(4), &
                bytes(5), bytes(6), bytes(7), bytes(8)], 0_int64)
        end function

        integer(kind=int16) function bytes_be_to_int_2(bytes, e)
        ! converts bytes in big-endian to an int16 based on requested endianness
        ! @param[in] e - .true. for little endian, .false. for big endian
            byte, dimension(2), intent(in) :: bytes
            logical, intent(in) :: e
            if (e) then
                bytes_be_to_int_2 = bytes_be_int_le_2(bytes)
            else
                bytes_be_to_int_2 = bytes_be_int_be_2(bytes)
            end if
        end function

        integer(kind=int32) function bytes_be_to_int_4(bytes, e)
        ! converts bytes in big-endian to an int16 based on requested endianness
        ! @param[in] e - .true. for little endian, .false. for big endian
            byte, dimension(4), intent(in) :: bytes
            logical, intent(in) :: e
            if (e) then
                bytes_be_to_int_4 = bytes_be_int_le_4(bytes)
            else
                bytes_be_to_int_4 = bytes_be_int_be_4(bytes)
            end if
        end function

        integer(kind=int64) function bytes_be_to_int_8(bytes, e)
        ! converts bytes in big-endian to an int16 based on requested endianness
        ! @param[in] e - .true. for little endian, .false. for big endian
            byte, dimension(8), intent(in) :: bytes
            logical, intent(in) :: e
            if (e) then
                bytes_be_to_int_8 = bytes_be_int_le_8(bytes)
            else
                bytes_be_to_int_8 = bytes_be_int_be_8(bytes)
            end if
        end function

        subroutine int_le_to_bytes_be_2(bytes, value)
        ! converts int16 little endian to bytes in big-endian
            byte, dimension(2), intent(inout) :: bytes
            integer(kind=int16), intent(in) :: value
            bytes(1) = int(ibits(value, 8, 8), kind=int8)
            bytes(2) = int(ibits(value, 0, 8), kind=int8)
        end subroutine

        subroutine int_le_to_bytes_be_4(bytes, value)
        ! converts int32 little endian to bytes in big-endian
            byte, dimension(4), intent(inout) :: bytes
            integer(kind=int32), intent(in) :: value
            bytes(1) = int(ibits(value, 24, 8), kind=int8)
            bytes(2) = int(ibits(value, 16, 8), kind=int8)
            bytes(3) = int(ibits(value,  8, 8), kind=int8)
            bytes(4) = int(ibits(value,  0, 8), kind=int8)
        end subroutine

        subroutine int_le_to_bytes_be_8(bytes, value)
        ! converts int64 little endian to bytes in big-endian
            byte, dimension(8), intent(inout) :: bytes
            integer(kind=int64), intent(in) :: value
            bytes(1) = int(ibits(value, 56, 8), kind=int8)
            bytes(2) = int(ibits(value, 48, 8), kind=int8)
            bytes(3) = int(ibits(value, 40, 8), kind=int8)
            bytes(4) = int(ibits(value, 32, 8), kind=int8)
            bytes(5) = int(ibits(value, 24, 8), kind=int8)
            bytes(6) = int(ibits(value, 16, 8), kind=int8)
            bytes(7) = int(ibits(value,  8, 8), kind=int8)
            bytes(8) = int(ibits(value,  0, 8), kind=int8)
        end subroutine

        subroutine int_be_to_bytes_be_2(bytes, value)
        ! converts int16 big endian to bytes in big-endian
            byte, dimension(2), intent(inout) :: bytes
            integer(kind=int16), intent(in) :: value
            bytes(1:2) = transfer(value, [0_int8, 0_int8])
        end subroutine

        subroutine int_be_to_bytes_be_4(bytes, value)
        ! converts int32 big endian to bytes in big-endian
            byte, dimension(4), intent(inout) :: bytes
            integer(kind=int32), intent(in) :: value
            bytes(1:4) = transfer(value, [0_int8, 0_int8, 0_int8, 0_int8])
        end subroutine

        subroutine int_be_to_bytes_be_8(bytes, value)
        ! converts int64 big endian to bytes in big-endian
            byte, dimension(8), intent(inout) :: bytes
            integer(kind=int64), intent(in) :: value
            bytes(1:8) = transfer(value, [0_int8, 0_int8, 0_int8, 0_int8, &
                0_int8, 0_int8, 0_int8, 0_int8])
        end subroutine

        subroutine int_to_bytes_be_2(bytes, value)
        ! converts int16 to bytes in big-endian based on requested endianness
        ! @param[in] e - .true. for little endian, .false. for big endian
            byte, dimension(2), intent(inout) :: bytes
            integer(kind=int16), intent(in) :: value
            if (detect_little_endian()) then
                call int_le_to_bytes_be_2(bytes, value)
            else
                call int_be_to_bytes_be_2(bytes, value)
            end if
        end subroutine

        subroutine int_to_bytes_be_4(bytes, value)
        ! converts int32 to bytes in big-endian based on requested endianness
        ! @param[in] e - .true. for little endian, .false. for big endian
            byte, dimension(4), intent(inout) :: bytes
            integer(kind=int32), intent(in) :: value
            if (detect_little_endian()) then
                call int_le_to_bytes_be_4(bytes, value)
            else
                call int_be_to_bytes_be_4(bytes, value)
            end if
        end subroutine

        subroutine int_to_bytes_be_8(bytes, value)
        ! converts int64 to bytes in big-endian based on requested endianness
        ! @param[in] e - .true. for little endian, .false. for big endian
            byte, dimension(8), intent(inout) :: bytes
            integer(kind=int64), intent(in) :: value
            if (detect_little_endian()) then
                call int_le_to_bytes_be_8(bytes, value)
            else
                call int_be_to_bytes_be_8(bytes, value)
            end if
        end subroutine
end module
