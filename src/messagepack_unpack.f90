module messagepack_unpack
    use iso_fortran_env
    use,intrinsic :: ieee_arithmetic
    use messagepack_value

    implicit none

    private

    public :: unpack_stream
    public :: detect_little_endian
    public :: print_endianness
    public :: bytes_be_int_le_2, bytes_be_int_le_4, bytes_be_int_le_8
    public :: bytes_be_int_be_2, bytes_be_int_be_4, bytes_be_int_be_8
    public :: bytes_be_to_int_2, bytes_be_to_int_4, bytes_be_to_int_8

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

        logical function check_length_and_print(need, actual)
            integer, intent(in) :: need
            integer, intent(in) :: actual
            if (actual < need) then
                print *, "Not enough bytes for type"
                check_length_and_print = .false.
            else
                check_length_and_print = .true.
            end if
        end function

        subroutine unpack_stream(buffer, mpv, successful)
            ! @param[in] buffer - input byte buffer containing messagepack data
            ! @param[out] successful - did unpacking succeed
            ! @param[out] error - If an error occurred, returns a message with why
            byte, allocatable, dimension(:), intent(in) :: buffer
            class(mp_value_type), allocatable, intent(out) :: mpv
            logical, intent(out) :: successful

            ! AFAIK there is no stdlib equivalent of C++20 std::endian
            logical :: little_endian
            
            little_endian = detect_little_endian()

            successful = .true.   ! initially set to true
            call unpack_value(buffer, little_endian, mpv, successful)
        end subroutine

        subroutine unpack_value(buffer, is_little_endian, mpv, successful)
            byte, allocatable, dimension(:), intent(in) :: buffer
            logical, intent(in) :: is_little_endian
            class(mp_value_type), allocatable, intent(out) :: mpv
            logical, intent(out) :: successful

            ! other variables to use
            integer :: length
            byte :: btemp1 ! byte temp value

            length = size(buffer)

            ! set default output values
            mpv = mp_nil_type()
            successful = .true.

            ! need to have data available to read
            if (length == 0) then
                successful = .false.
                print *, "Buffer is empty"
                return
            end if

            select case (buffer(1))
            case (MP_PFI_L:MP_PFI_H)
                ! the byte itself is the value
                mpv = mp_int_type(buffer(1))
            case (MP_FM_L:MP_FM_H)
                print *, "Fixmap"
            case (MP_FA_L:MP_FA_H)
                print *, "Fixarray"
            case (MP_FS_L:MP_FS_H)
                print *, "Fixstr"
            case (MP_NIL)
                ! default is already nil
                return
            case (MP_NU)
                print *, "Error, never used detected"
                successful = .false.
            case (MP_F)
                mpv = mp_bool_type(.false.)
            case (MP_T)
                mpv = mp_bool_type(.true.)
            ! binary format family
            case (MP_B8)
                ! 1 byte to describe the number of data bytes
                if (check_length_and_print(2, length)) then
                    successful = .false.
                    return
                end if
                btemp1 = buffer(2)
                print *, "Bin8"
            case (MP_B16)
                print *, "Bin16"
            case (MP_B32)
                print *, "Bin32"
            case (MP_E8)
                print *, "Ext8"
            case (MP_E16)
                print *, "Ext16"
            case (MP_E32)
                print *, "Ext32"
            case (MP_F32)
                mpv = new_real32(0.0)
            case (MP_F64)
            ! Unsigned integers >>>
            ! need to watch when grabbed values are negative
            case (MP_U8)
                ! 1 byte following
                if (check_length_and_print(2, length)) then
                    successful = .false.
                    return
                end if
                btemp1 = buffer(2)
                if (btemp1 < 0) then
                    ! negative, reinterpret bits
                else
                    ! positive, good to use immediately
                    mpv = mp_int_type(btemp1)
                end if
            case (MP_U16)
            case (MP_U32)
            case (MP_U64)
            ! Signed integers >>>
            case (MP_I8)
                ! 1 byte following
                if (check_length_and_print(2, length)) then
                    successful = .false.
                    return
                end if
                mpv = mp_int_type(buffer(2))
            case (MP_I16)
                ! 2 bytes following
                if (check_length_and_print(3, length)) then
                    successful = .false.
                    return
                end if
                mpv = mp_int_type(bytes_be_to_int_2(buffer(2:3), is_little_endian))
            case (MP_I32)
                ! 4 bytes following
                if (check_length_and_print(5, length)) then
                    successful = .false.
                    return
                end if
                mpv = mp_int_type(bytes_be_to_int_4(buffer(2:5), is_little_endian))
            case (MP_I64)
                ! 8 bytes following
                if (check_length_and_print(9, length)) then
                    successful = .false.
                    return
                end if
                mpv = mp_int_type(bytes_be_to_int_8(buffer(2:9), is_little_endian))
            ! ext format family
            case (MP_FE1)
            case (MP_FE2)
            case (MP_FE4)
            case (MP_FE8)
            case (MP_FE16)
            case (MP_S8)
            case (MP_S16)
            case (MP_S32)
            case (MP_A16)
            case (MP_A32)
            case (MP_M16)
            case (MP_M32)
            case (MP_NFI_L:MP_NFI_H)
                ! take the first 5 bits, create a negative value from it
                btemp1 = 0
                call mvbits(buffer(1), 0, 5, btemp1, 0)
                mpv = mp_int_type(-btemp1)
            end select
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
end module
