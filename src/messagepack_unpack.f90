module messagepack_unpack
    use iso_fortran_env
    use,intrinsic :: ieee_arithmetic
    use byte_utilities
    use messagepack_value

    implicit none

    private

    public :: unpack_stream

    contains

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
            integer(kind=int16) :: val_int16
            integer(kind=int32) :: val_int32
            integer(kind=int64) :: val_int64

            length = size(buffer)

            ! set default output values
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
                if (.not. check_length_and_print(2, length)) then
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
                if (.not. check_length_and_print(2, length)) then
                    successful = .false.
                    return
                end if
                btemp1 = buffer(2)
                if (btemp1 >= 0) then
                    mpv = mp_int_type(btemp1)
                else
                    mpv = mp_int_type(255_int16 + btemp1)
                end if
            case (MP_U16)
                ! 2 bytes following
                if (.not. check_length_and_print(3, length)) then
                    successful = .false.
                    return
                end if
                val_int16 = bytes_be_to_int_2(buffer(2:3), is_little_endian)
                if (val_int16 >= 0) then
                    mpv = mp_int_type(val_int16)
                else
                    mpv = mp_int_type(65536_int32 + val_int16)
                end if
            case (MP_U32)
                ! 4 bytes following
                if (.not. check_length_and_print(5, length)) then
                    successful = .false.
                    return
                end if
                val_int32 = bytes_be_to_int_4(buffer(2:5), is_little_endian)
                if (val_int32 >= 0) then
                    mpv = mp_int_type(val_int32)
                else
                    mpv = mp_int_type(4294967296_int64 + val_int32)
                end if
            case (MP_U64)
                ! 8 bytes following
                if (.not. check_length_and_print(9, length)) then
                    successful = .false.
                    return
                end if
                val_int64 = bytes_be_to_int_8(buffer(2:9), is_little_endian)
                if (val_int64 >= 0) then
                    mpv = mp_int_type(val_int64)
                else
                    mpv = mp_int_type(val_int64)
                    call set_unsigned(mpv)
                end if
            ! Signed integers >>>
            case (MP_I8)
                ! 1 byte following
                if (.not. check_length_and_print(2, length)) then
                    successful = .false.
                    return
                end if
                mpv = mp_int_type(buffer(2))
            case (MP_I16)
                ! 2 bytes following
                if (.not. check_length_and_print(3, length)) then
                    successful = .false.
                    return
                end if
                mpv = mp_int_type(bytes_be_to_int_2(buffer(2:3), is_little_endian))
            case (MP_I32)
                ! 4 bytes following
                if (.not. check_length_and_print(5, length)) then
                    successful = .false.
                    return
                end if
                mpv = mp_int_type(bytes_be_to_int_4(buffer(2:5), is_little_endian))
            case (MP_I64)
                ! 8 bytes following
                if (.not. check_length_and_print(9, length)) then
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
                btemp1 = ibits(buffer(1), 0, 5)
                mpv = mp_int_type(-btemp1)
            end select
        end subroutine
end module
