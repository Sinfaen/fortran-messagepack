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
            integer(kind=int64), intent(in) :: need
            integer(kind=int64), intent(in) :: actual
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
            byte, dimension(:), intent(in) :: buffer
            class(mp_value_type), allocatable, intent(out) :: mpv
            integer(kind=int64) :: byteadvance
            logical, intent(out) :: successful

            ! AFAIK there is no stdlib equivalent of C++20 std::endian
            logical :: little_endian
            
            little_endian = detect_little_endian()

            successful = .true.   ! initially set to true
            call unpack_value(buffer, byteadvance, little_endian, mpv, successful)
        end subroutine

        recursive subroutine unpack_value(buffer, byteadvance, is_little_endian, mpv, successful)
            byte, dimension(:), intent(in) :: buffer
            integer(kind=int64), intent(out) :: byteadvance
            logical, intent(in) :: is_little_endian
            class(mp_value_type), allocatable, intent(out) :: mpv
            logical, intent(out) :: successful

            ! other variables to use
            integer(kind=int64) :: length
            integer :: i
            integer(kind=int64) :: i_64
            byte :: btemp1 ! byte temp value
            integer(kind=int16) :: val_int16
            integer(kind=int32) :: val_int32
            integer(kind=int64) :: val_int64

            integer(kind=int64) :: header_size
            character(:), allocatable :: val_char

            length = size(buffer)

            ! set default output values
            successful = .true.

            ! need to have data available to read
            if (length == 0) then
                successful = .false.
                print *, "Buffer is empty"
                return
            end if

            ! check that the size for the entire header exists
            header_size = get_header_size_by_type(buffer(1))
            if (.not. check_length_and_print(header_size, length)) then
                successful = .false.
                return
            end if

            byteadvance = 1 ! default output value
            select case (buffer(1))
            case (MP_PFI_L:MP_PFI_H)
                ! the byte itself is the value
                mpv = mp_int_type(buffer(1))
            case (MP_FM_L:MP_FM_H)
                btemp1 = 0
                call mvbits(buffer(1), 0, 4, btemp1, 0) ! get fixmap length
                val_int64 = btemp1
                if (.not. check_length_and_print(1 + val_int64, length)) then
                    successful = .false.
                    return
                end if
                byteadvance = 2 ! start at next object
                call unpack_map(val_int64, buffer, byteadvance, is_little_endian, mpv, successful)
            case (MP_FA_L:MP_FA_H)
                btemp1 = 0
                call mvbits(buffer(1), 0, 4, btemp1, 0) ! get fixarray length
                if (.not. check_length_and_print(1_int64 + btemp1, length)) then
                    successful = .false.
                    return
                end if
                byteadvance = 2 ! start at next object
                call unpack_array(btemp1 + 0_int64, buffer, byteadvance, is_little_endian, mpv, successful)
            case (MP_FS_L:MP_FS_H)
                btemp1 = 0
                call mvbits(buffer(1), 0, 5, btemp1, 0) ! get fixstr length
                if (.not. check_length_and_print(1_int64 + btemp1, length)) then
                    successful = .false.
                    return
                end if
                allocate(character(btemp1) :: val_char)
                do i = 1,btemp1
                    val_char(i:i) = transfer(buffer(1 + i), 'a')
                end do
                mpv = mp_str_type(val_char)
                byteadvance = 1 + btemp1
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
                ! check that the remaining number of bytes exist
                val_int32 = int8_as_unsigned(buffer(2))
                val_int64 = val_int32
                if (.not. check_length_and_print(1 + val_int64, length)) then
                    successful = .false.
                    return
                end if
                mpv = mp_bin_type(val_int64)
                ! copy data
                select type (mpv)
                type is (mp_value_type)
                class is (mp_bin_type)
                    mpv%value(:) = buffer(3:)
                class default
                    successful = .false.
                    print *, "[Error: something went terribly wrong"
                end select
            case (MP_B16)
                ! check that the remaining number of bytes exist
                val_int16 = bytes_be_to_int_2(buffer(2:3), is_little_endian)
                val_int64 = int16_as_unsigned(val_int16)
                if (.not. check_length_and_print(1 + val_int64, length)) then
                    successful = .false.
                    return
                end if
                mpv = mp_bin_type(val_int64)
                ! copy data
                select type (mpv)
                type is (mp_value_type)
                class is (mp_bin_type)
                    mpv%value(:) = buffer(4:)
                class default
                    successful = .false.
                    print *, "[Error: something went terribly wrong"
                end select
            case (MP_B32)
                ! check that the remaining number of bytes exist
                val_int32 = bytes_be_to_int_4(buffer(2:5), is_little_endian)
                val_int64 = int32_as_unsigned(val_int32)
                if (.not. check_length_and_print(1 + val_int64, length)) then
                    successful = .false.
                    return
                end if
                mpv = mp_bin_type(val_int64)
                ! copy data
                select type (mpv)
                type is (mp_value_type)
                class is (mp_bin_type)
                    mpv%value(:) = buffer(6:)
                class default
                    successful = .false.
                    print *, "[Error: something went terribly wrong"
                end select
            case (MP_E8)
                ! check for first 3 bytes
                if (.not. check_length_and_print(3_int64, length)) then
                    successful = .false.
                    return
                end if
                i = buffer(3)
                call unpack_ext(int8_as_unsigned(buffer(2)) + 0_int64, i, buffer(4:), byteadvance, mpv, successful)
            case (MP_E16)
                ! check for first 4 bytes
                if (.not. check_length_and_print(4_int64, length)) then
                    successful = .false.
                    return
                end if
                i = buffer(4)
                val_int16 = bytes_be_to_int_2(buffer(2:3), is_little_endian)
                call unpack_ext(val_int16 + 0_int64, i, buffer(5:), byteadvance, mpv, successful)
            case (MP_E32)
                ! check for first 6 bytes
                if (.not. check_length_and_print(6_int64, length)) then
                    successful = .false.
                    return
                end if
                i = buffer(6)
                val_int32 = bytes_be_to_int_4(buffer(2:5), is_little_endian)
                call unpack_ext(val_int32 + 0_int64, i, buffer(7:), byteadvance, mpv, successful)
            case (MP_F32)
                ! 4 bytes following
                if (.not. check_length_and_print(5_int64, length)) then
                    successful = .false.
                    return
                end if
                mpv = new_real32(bytes_be_to_real_4(buffer(2:5), is_little_endian))
                byteadvance = 5
            case (MP_F64)
                ! 8 bytes following
                if (.not. check_length_and_print(9_int64, length)) then
                    successful = .false.
                    return
                end if
                mpv = new_real64(bytes_be_to_real_8(buffer(2:9), is_little_endian))
                byteadvance = 9
            ! Unsigned integers >>>
            ! need to watch when grabbed values are negative
            case (MP_U8)
                ! 1 byte following
                if (.not. check_length_and_print(2_int64, length)) then
                    successful = .false.
                    return
                end if
                mpv = mp_int_type(int8_as_unsigned(buffer(2)))
                byteadvance = 2
            case (MP_U16)
                ! 2 bytes following
                if (.not. check_length_and_print(3_int64, length)) then
                    successful = .false.
                    return
                end if
                val_int16 = bytes_be_to_int_2(buffer(2:3), is_little_endian)
                mpv = mp_int_type(int16_as_unsigned(val_int16))
                byteadvance = 3
            case (MP_U32)
                ! 4 bytes following
                if (.not. check_length_and_print(5_int64, length)) then
                    successful = .false.
                    return
                end if
                val_int32 = bytes_be_to_int_4(buffer(2:5), is_little_endian)
                mpv = mp_int_type(int32_as_unsigned(val_int32))
                byteadvance = 5
            case (MP_U64)
                ! 8 bytes following
                if (.not. check_length_and_print(9_int64, length)) then
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
                byteadvance = 9
            ! Signed integers >>>
            case (MP_I8)
                ! 1 byte following
                if (.not. check_length_and_print(2_int64, length)) then
                    successful = .false.
                    return
                end if
                mpv = mp_int_type(buffer(2))
                byteadvance = 2
            case (MP_I16)
                ! 2 bytes following
                if (.not. check_length_and_print(3_int64, length)) then
                    successful = .false.
                    return
                end if
                val_int16 = bytes_be_to_int_2(buffer(2:3), is_little_endian)
                val_int32 = int16_as_unsigned(val_int16)
                mpv = mp_int_type(val_int32)
                byteadvance = 3
            case (MP_I32)
                ! 4 bytes following
                if (.not. check_length_and_print(5_int64, length)) then
                    successful = .false.
                    return
                end if
                mpv = mp_int_type(bytes_be_to_int_4(buffer(2:5), is_little_endian))
                byteadvance = 5
            case (MP_I64)
                ! 8 bytes following
                if (.not. check_length_and_print(9_int64, length)) then
                    successful = .false.
                    return
                end if
                mpv = mp_int_type(bytes_be_to_int_8(buffer(2:9), is_little_endian))
                byteadvance = 9
            ! ext format family
            case (MP_FE1)
                ! 3 bytes following
                if (.not. check_length_and_print(3_int64, length)) then
                    successful = .false.
                    return
                end if
                i = buffer(2)
                byteadvance = 2
                call unpack_ext(1_int64, i, buffer(3:), byteadvance, mpv, successful)
            case (MP_FE2)
                ! 4 bytes following
                if (.not. check_length_and_print(4_int64, length)) then
                    successful = .false.
                    return
                end if
                i = buffer(2)
                byteadvance = 2
                call unpack_ext(2_int64, i, buffer(3:), byteadvance, mpv, successful)
            case (MP_FE4)
                ! 6 bytes following
                if (.not. check_length_and_print(6_int64, length)) then
                    successful = .false.
                    return
                end if
                i = buffer(2)
                byteadvance = 2
                call unpack_ext(4_int64, i, buffer(3:), byteadvance, mpv, successful)
            case (MP_FE8)
                ! 8 bytes following
                if (.not. check_length_and_print(8_int64, length)) then
                    successful = .false.
                    return
                end if
                i = buffer(2)
                byteadvance = 2
                call unpack_ext(8_int64, i, buffer(3:), byteadvance, mpv, successful)
            case (MP_FE16)
                ! 18 bytes following
                if (.not. check_length_and_print(18_int64, length)) then
                    successful = .false.
                    return
                end if
                i = buffer(2)
                byteadvance = 2
                call unpack_ext(16_int64, i, buffer(3:), byteadvance, mpv, successful)
            case (MP_S8)
                ! check that the remaining number of bytes exist
                val_int16 = int8_as_unsigned(buffer(2))
                if (.not. check_length_and_print(2_int64 + val_int16, length)) then
                    successful = .false.
                    return
                end if
                ! create string
                allocate(character(val_int16) :: val_char)
                do i = 1,val_int16
                    val_char(i:i) = transfer(buffer(2 + i), 'a')
                end do
                mpv = mp_str_type(val_char)
                byteadvance = 1 + val_int16
            case (MP_S16)
                ! check that the remaining number of bytes exist
                val_int16 = bytes_be_to_int_2(buffer(2:3), is_little_endian)
                val_int32 = int16_as_unsigned(val_int16)
                if (.not. check_length_and_print(3_int64 + val_int32, length)) then
                    successful = .false.
                    return
                end if
                ! create string
                allocate(character(val_int32) :: val_char)
                do i = 1,val_int32
                    val_char(i:i) = transfer(buffer(3 + i), 'a')
                end do
                mpv = mp_str_type(val_char)
                byteadvance = 1 + val_int32
            case (MP_S32)
                ! check that the remaining number of bytes exist
                val_int32 = bytes_be_to_int_4(buffer(2:5), is_little_endian)
                val_int64 = int32_as_unsigned(val_int32)
                if (5 + val_int64 > length) then
                    successful = .false.
                    return
                end if
                ! create string
                allocate(character(val_int64) :: val_char)
                do i_64 = 1_int64,val_int64
                    val_char(i_64:i_64) = transfer(buffer(3 + i_64), 'a')
                end do
                mpv = mp_str_type(val_char)
                byteadvance = 1_int64 + val_int64
            case (MP_A16)
                ! check that the remaining number of bytes exist
                val_int16 = bytes_be_to_int_2(buffer(2:3), is_little_endian)
                val_int32 = int16_as_unsigned(val_int16)
                if (.not. check_length_and_print(1_int64 + val_int32, length)) then
                    successful = .false.
                    return
                end if
                byteadvance = 4 ! start at next object
                call unpack_array(int(val_int32, kind=int64), buffer, byteadvance, is_little_endian, mpv, successful)
            case (MP_A32)
                ! check that the remaining number of bytes exist
                val_int32 = bytes_be_to_int_4(buffer(2:5), is_little_endian)
                val_int64 = int32_as_unsigned(val_int32)
                if (.not. check_length_and_print(1 + val_int64, length)) then
                    successful = .false.
                    return
                end if
                byteadvance = 6 ! start at next object
                call unpack_array(val_int64, buffer, byteadvance, is_little_endian, mpv, successful)
            case (MP_M16)
                ! check that the remaining number of bytes exist
                val_int16 = bytes_be_to_int_2(buffer(2:3), is_little_endian)
                val_int32 = int16_as_unsigned(val_int16)
                if (.not. check_length_and_print(1_int64 + val_int32, length)) then
                    successful = .false.
                    return
                end if
                byteadvance = 4 ! start at next object
                call unpack_map(0_int64 + val_int32, buffer, byteadvance, is_little_endian, mpv, successful)
            case (MP_M32)
                ! check that the remaining number of bytes exist
                val_int32 = bytes_be_to_int_4(buffer(2:5), is_little_endian)
                val_int64 = int32_as_unsigned(val_int32)
                if (.not. check_length_and_print(1 + val_int64, length)) then
                    successful = .false.
                    return
                end if
                byteadvance = 6 ! start at next object
                call unpack_map(val_int64, buffer, byteadvance, is_little_endian, mpv, successful)
            case (MP_NFI_L:MP_NFI_H)
                ! take the first 5 bits, create a negative value from it
                btemp1 = ibits(buffer(1), 0, 5)
                mpv = mp_int_type(-btemp1)
            end select
        end subroutine

        recursive subroutine unpack_array(length, buffer, byteadvance, is_little_endian, mpv, successful)
            integer(kind=int64), intent(in) :: length
            byte, dimension(:), intent(in) :: buffer
            integer(kind=int64), intent(inout) :: byteadvance
            logical, intent(in) :: is_little_endian
            class(mp_value_type), allocatable, intent(out) :: mpv
            logical, intent(out) :: successful

            integer(kind=int64) :: i, tmp
            class(mp_value_type), allocatable :: val_any
            mpv = mp_arr_type(length)
            do i = 1,length
                call unpack_value(buffer(byteadvance:), tmp, is_little_endian, val_any, successful)
                byteadvance = byteadvance + tmp
                if (.not. successful) then
                    return
                end if

                ! store the newly unpacked object into the array
                select type (mpv)
                type is (mp_value_type)
                class is (mp_arr_type)
                    mpv%value(i)%obj = val_any
                class default
                    successful = .false.
                    print *, "[Error: something went terribly wrong"
                end select
            end do
        end subroutine

        recursive subroutine unpack_map(length, buffer, byteadvance, is_little_endian, mpv, successful)
            integer(kind=int64), intent(in) :: length
            byte, dimension(:), intent(in) :: buffer
            integer(kind=int64), intent(inout) :: byteadvance
            logical, intent(in) :: is_little_endian
            class(mp_value_type), allocatable, intent(out) :: mpv
            logical, intent(out) :: successful

            integer(kind=int64) :: i, tmp
            class(mp_value_type), allocatable :: val_any

            successful = .true.
            mpv = mp_map_type(length)
            do i = 1,length
                ! get key
                call unpack_value(buffer(byteadvance:), tmp, is_little_endian, val_any, successful)
                byteadvance = byteadvance + tmp
                if (.not. successful) then
                    return
                end if
                select type (mpv)
                type is (mp_value_type)
                class is (mp_map_type)
                    mpv%keys(i)%obj = val_any
                class default
                    successful = .false.
                    print *, "[Error: something went terribly wrong"
                end select

                ! get value
                call unpack_value(buffer(byteadvance:), tmp, is_little_endian, val_any, successful)
                byteadvance = byteadvance + tmp
                if (.not. successful) then
                    return
                end if
                select type (mpv)
                type is (mp_value_type)
                class is (mp_map_type)
                    mpv%values(i)%obj = val_any
                class default
                    successful = .false.
                    print *, "[Error: something went terribly wrong"
                end select
            end do
        end subroutine

        subroutine unpack_ext(length, etype, buffer, byteadvance, mpv, successful)
            integer(kind=int64), intent(in) :: length
            integer, intent(in) :: etype
            byte, dimension(:), intent(in) :: buffer
            integer(kind=int64), intent(inout) :: byteadvance
            class(mp_value_type), allocatable, intent(out) :: mpv
            logical, intent(out) :: successful

            if (length > size(buffer)) then
                successful = .false.
                return
            end if

            mpv = mp_ext_type(etype, length)
            successful = .true.
            select type(mpv)
            type is (mp_value_type)
            class is (mp_ext_type)
                mpv%values = buffer(1:length)
                byteadvance = byteadvance + length
            class default
                successful = .false.
                print *, "[Error: something went terribly wrong"
            end select
        end subroutine
end module
