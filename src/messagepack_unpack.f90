module messagepack_unpack
    use iso_fortran_env
    use, intrinsic :: ieee_arithmetic
    use byte_utilities
    use messagepack_value
    use messagepack_user

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

        subroutine unpack_stream(settings, buffer, mpv, successful)
            ! @param[in] buffer - input byte buffer containing messagepack data
            ! @param[out] successful - did unpacking succeed
            ! @param[out] error - If an error occurred, returns a message with why
            class(mp_settings), intent(in) :: settings
            byte, dimension(:), intent(in) :: buffer
            class(mp_value_type), allocatable, intent(out) :: mpv
            integer(kind=int64) :: byteadvance
            logical, intent(out) :: successful

            successful = .true.   ! initially set to true
            call unpack_value(settings, buffer, byteadvance, mpv, successful)

            if (byteadvance < size(buffer)) then
                print *, "[Warning: Extra", size(buffer) - byteadvance, "bytes unused"
            else if (byteadvance > size(buffer)) then
                successful = .false. ! bug within reporting byte mechanism
                print *, "[Error: internal error byteadvance beyond buffer length"
                print *, byteadvance, "=/=", size(buffer)
            end if
        end subroutine

        recursive subroutine unpack_value(settings, buffer, byteadvance, &
                mpv, successful)
            class(mp_settings), intent(in) :: settings
            byte, dimension(:), intent(in) :: buffer
            integer(kind=int64), intent(out) :: byteadvance
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
                byteadvance = 1
                call unpack_map(settings, val_int64, buffer, byteadvance, &
                    mpv, successful)
            case (MP_FA_L:MP_FA_H)
                btemp1 = 0
                call mvbits(buffer(1), 0, 4, btemp1, 0) ! get fixarray length
                if (.not. check_length_and_print(1_int64 + btemp1, length)) then
                    successful = .false.
                    return
                end if
                byteadvance = 1
                call unpack_array(settings, btemp1 + 0_int64, buffer, byteadvance, &
                    mpv, successful)
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
                mpv = mp_nil_type()
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
                    mpv%value(:) = buffer(3:2+val_int64)
                class default
                    successful = .false.
                    print *, "[Error: something went terribly wrong"
                end select
                byteadvance = 2 + val_int64
            case (MP_B16)
                ! check that the remaining number of bytes exist
                val_int16 = bytes_be_to_int_2(buffer(2:3), settings%is_little_endian)
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
                    mpv%value(:) = buffer(4:3+val_int64)
                class default
                    successful = .false.
                    print *, "[Error: something went terribly wrong"
                end select
                byteadvance = 3 + val_int64
            case (MP_B32)
                ! check that the remaining number of bytes exist
                val_int32 = bytes_be_to_int_4(buffer(2:5), settings%is_little_endian)
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
                    mpv%value(:) = buffer(6:5+val_int64)
                class default
                    successful = .false.
                    print *, "[Error: something went terribly wrong"
                end select
                byteadvance = 5 + val_int64
            case (MP_E8)
                ! check for first 3 bytes
                if (.not. check_length_and_print(3_int64, length)) then
                    successful = .false.
                    return
                end if
                i = buffer(3)
                byteadvance = 3
                call unpack_ext(settings, int8_as_unsigned(buffer(2)) + 0_int64, &
                    i, buffer, byteadvance, mpv, successful)
            case (MP_E16)
                ! check for first 4 bytes
                if (.not. check_length_and_print(4_int64, length)) then
                    successful = .false.
                    return
                end if
                i = buffer(4)
                byteadvance = 4
                val_int16 = bytes_be_to_int_2(buffer(2:3), settings%is_little_endian)
                call unpack_ext(settings, val_int16 + 0_int64, &
                    i, buffer, byteadvance, mpv, successful)
            case (MP_E32)
                ! check for first 6 bytes
                if (.not. check_length_and_print(6_int64, length)) then
                    successful = .false.
                    return
                end if
                i = buffer(6)
                byteadvance = 6
                val_int32 = bytes_be_to_int_4(buffer(2:5), settings%is_little_endian)
                call unpack_ext(settings, val_int32 + 0_int64, &
                    i, buffer, byteadvance, mpv, successful)
            case (MP_F32)
                ! 4 bytes following
                if (.not. check_length_and_print(5_int64, length)) then
                    successful = .false.
                    return
                end if
                mpv = new_real32(bytes_be_to_real_4(buffer(2:5), &
                    settings%is_little_endian))
                byteadvance = 5
            case (MP_F64)
                ! 8 bytes following
                if (.not. check_length_and_print(9_int64, length)) then
                    successful = .false.
                    return
                end if
                mpv = new_real64(bytes_be_to_real_8(buffer(2:9), settings%is_little_endian))
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
                val_int16 = bytes_be_to_int_2(buffer(2:3), settings%is_little_endian)
                mpv = mp_int_type(int16_as_unsigned(val_int16))
                byteadvance = 3
            case (MP_U32)
                ! 4 bytes following
                if (.not. check_length_and_print(5_int64, length)) then
                    successful = .false.
                    return
                end if
                val_int32 = bytes_be_to_int_4(buffer(2:5), settings%is_little_endian)
                mpv = mp_int_type(int32_as_unsigned(val_int32))
                byteadvance = 5
            case (MP_U64)
                ! 8 bytes following
                if (.not. check_length_and_print(9_int64, length)) then
                    successful = .false.
                    return
                end if
                val_int64 = bytes_be_to_int_8(buffer(2:9), settings%is_little_endian)
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
                val_int16 = bytes_be_to_int_2(buffer(2:3), settings%is_little_endian)
                val_int32 = int16_as_unsigned(val_int16)
                mpv = mp_int_type(val_int32)
                byteadvance = 3
            case (MP_I32)
                ! 4 bytes following
                if (.not. check_length_and_print(5_int64, length)) then
                    successful = .false.
                    return
                end if
                mpv = mp_int_type(bytes_be_to_int_4(buffer(2:5), settings%is_little_endian))
                byteadvance = 5
            case (MP_I64)
                ! 8 bytes following
                if (.not. check_length_and_print(9_int64, length)) then
                    successful = .false.
                    return
                end if
                mpv = mp_int_type(bytes_be_to_int_8(buffer(2:9), settings%is_little_endian))
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
                call unpack_ext(settings, 1_int64, &
                    i, buffer, byteadvance, mpv, successful)
            case (MP_FE2)
                ! 4 bytes following
                if (.not. check_length_and_print(4_int64, length)) then
                    successful = .false.
                    return
                end if
                i = buffer(2)
                byteadvance = 2
                call unpack_ext(settings, 2_int64, &
                    i, buffer, byteadvance, mpv, successful)
            case (MP_FE4)
                ! 6 bytes following
                if (.not. check_length_and_print(6_int64, length)) then
                    successful = .false.
                    return
                end if
                i = buffer(2)
                byteadvance = 2
                call unpack_ext(settings, 4_int64, &
                    i, buffer, byteadvance, mpv, successful)
            case (MP_FE8)
                ! 8 bytes following
                if (.not. check_length_and_print(8_int64, length)) then
                    successful = .false.
                    return
                end if
                i = buffer(2)
                byteadvance = 2
                call unpack_ext(settings, 8_int64, &
                    i, buffer, byteadvance, mpv, successful)
            case (MP_FE16)
                ! 18 bytes following
                if (.not. check_length_and_print(18_int64, length)) then
                    successful = .false.
                    return
                end if
                i = buffer(2)
                byteadvance = 2
                call unpack_ext(settings, 16_int64, &
                    i, buffer, byteadvance, mpv, successful)
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
                val_int16 = bytes_be_to_int_2(buffer(2:3), settings%is_little_endian)
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
                val_int32 = bytes_be_to_int_4(buffer(2:5), settings%is_little_endian)
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
                val_int16 = bytes_be_to_int_2(buffer(2:3), settings%is_little_endian)
                val_int32 = int16_as_unsigned(val_int16)
                if (.not. check_length_and_print(1_int64 + val_int32, length)) then
                    successful = .false.
                    return
                end if
                byteadvance = 3
                call unpack_array(settings, int(val_int32, kind=int64), &
                    buffer, byteadvance, mpv, successful)
            case (MP_A32)
                ! check that the remaining number of bytes exist
                val_int32 = bytes_be_to_int_4(buffer(2:5), settings%is_little_endian)
                val_int64 = int32_as_unsigned(val_int32)
                if (.not. check_length_and_print(1 + val_int64, length)) then
                    successful = .false.
                    return
                end if
                byteadvance = 5
                call unpack_array(settings, val_int64, buffer, byteadvance, &
                    mpv, successful)
            case (MP_M16)
                ! check that the remaining number of bytes exist
                val_int16 = bytes_be_to_int_2(buffer(2:3), settings%is_little_endian)
                val_int32 = int16_as_unsigned(val_int16)
                if (.not. check_length_and_print(1_int64 + val_int32, length)) then
                    successful = .false.
                    return
                end if
                byteadvance = 3
                call unpack_map(settings, 0_int64 + val_int32, buffer, byteadvance, &
                    mpv, successful)
            case (MP_M32)
                ! check that the remaining number of bytes exist
                val_int32 = bytes_be_to_int_4(buffer(2:5), settings%is_little_endian)
                val_int64 = int32_as_unsigned(val_int32)
                if (.not. check_length_and_print(1 + val_int64, length)) then
                    successful = .false.
                    return
                end if
                byteadvance = 5
                call unpack_map(settings, val_int64, buffer, byteadvance, &
                    mpv, successful)
            case (MP_NFI_L:MP_NFI_H)
                ! take the first 5 bits, create a negative value from it
                btemp1 = ibits(buffer(1), 0, 5)
                mpv = mp_int_type(-btemp1)
            end select
        end subroutine

        recursive subroutine unpack_array(settings, length, buffer, &
                byteadvance, mpv, successful)
            class(mp_settings), intent(in) :: settings
            integer(kind=int64), intent(in) :: length
            byte, dimension(:), intent(in) :: buffer
            integer(kind=int64), intent(inout) :: byteadvance
            class(mp_value_type), allocatable, intent(out) :: mpv
            logical, intent(out) :: successful

            integer(kind=int64) :: i, tmp
            class(mp_value_type), allocatable :: val_any
            mpv = mp_arr_type(length)
            do i = 1,length
                call unpack_value(settings, buffer(byteadvance+1:), tmp, &
                    val_any, successful)
                byteadvance = byteadvance + tmp
                if (.not. successful) then
                    deallocate(mpv)
                    return
                end if

                ! store the newly unpacked object into the array
                select type (mpv)
                type is (mp_value_type)
                class is (mp_arr_type)
                    mpv%value(i)%obj = val_any
                class default
                    successful = .false.
                    deallocate(mpv)
                    print *, "[Error: something went terribly wrong"
                end select
            end do
        end subroutine

        recursive subroutine unpack_map(settings, length, buffer, byteadvance, &
                mpv, successful)
            class(mp_settings), intent(in) :: settings
            integer(kind=int64), intent(in) :: length
            byte, dimension(:), intent(in) :: buffer
            integer(kind=int64), intent(inout) :: byteadvance
            class(mp_value_type), allocatable, intent(out) :: mpv
            logical, intent(out) :: successful

            integer(kind=int64) :: i, tmp
            class(mp_value_type), allocatable :: val_any

            successful = .true.
            mpv = mp_map_type(length)
            do i = 1,length
                ! get key
                call unpack_value(settings, buffer(byteadvance+1:), &
                    tmp, val_any, successful)
                byteadvance = byteadvance + tmp
                if (.not. successful) then
                    deallocate(mpv)
                    return
                end if
                select type (mpv)
                type is (mp_value_type)
                class is (mp_map_type)
                    mpv%keys(i)%obj = val_any
                class default
                    successful = .false.
                    deallocate(mpv)
                    print *, "[Error: something went terribly wrong"
                end select

                ! get value
                call unpack_value(settings, buffer(byteadvance+1:), tmp, &
                    val_any, successful)
                byteadvance = byteadvance + tmp
                if (.not. successful) then
                    deallocate(mpv)
                    return
                end if
                select type (mpv)
                type is (mp_value_type)
                class is (mp_map_type)
                    mpv%values(i)%obj = val_any
                class default
                    successful = .false.
                    deallocate(mpv)
                    print *, "[Error: something went terribly wrong"
                end select
            end do
        end subroutine

        subroutine unpack_ext(settings, length, etype, buffer, byteadvance, &
                mpv, successful)
            class(mp_settings), intent(in) :: settings
            integer(kind=int64), intent(in) :: length
            integer, intent(in) :: etype
            byte, dimension(:), intent(in) :: buffer
            integer(kind=int64), intent(inout) :: byteadvance
            class(mp_value_type), allocatable, intent(out) :: mpv
            logical, intent(out) :: successful

            integer :: ind
            if (length > size(buffer)) then
                successful = .false.
                return
            end if

            ! Custom extension handling
            ind = etype + 129
            if (ind < 1 .or. ind > 256) then
                successful = .false.
                return
            end if
            if (length == 1) then
                if (settings%f1_allocated(ind)) then
                    call settings%f1(ind)%cb(buffer, byteadvance, &
                        settings%is_little_endian, mpv, successful)
                    return
                end if
            else if (length == 2) then
                if (settings%f2_allocated(ind)) then
                    call settings%f2(ind)%cb(buffer, byteadvance, &
                        settings%is_little_endian, mpv, successful)
                    return
                end if
            else if (length == 4) then
                if (settings%f4_allocated(ind)) then
                    call settings%f4(ind)%cb(buffer, byteadvance, &
                        settings%is_little_endian, mpv, successful)
                    return
                end if
            else if (length == 8) then
                if (settings%f8_allocated(ind)) then
                    call settings%f8(ind)%cb(buffer, byteadvance, &
                        settings%is_little_endian, mpv, successful)
                    return
                end if
            else if (length == 16) then
                if (settings%f16_allocated(ind)) then
                    call settings%f16(ind)%cb(buffer, byteadvance, &
                        settings%is_little_endian, mpv, successful)
                        return
                end if
            else if (length < 256) then
                if (settings%e8_allocated(ind)) then
                    call settings%e8(ind)%cb(buffer, byteadvance, &
                        settings%is_little_endian, mpv, successful)
                        return
                end if
            else if (length < 65536) then
                if (settings%e16_allocated(ind)) then
                    call settings%e16(ind)%cb(buffer, byteadvance, &
                        settings%is_little_endian, mpv, successful)
                        return
                end if
            else if (length < 4294967296_int64) then
                if (settings%e32_allocated(ind)) then
                    call settings%e32(ind)%cb(buffer, byteadvance, &
                        settings%is_little_endian, mpv, successful)
                        return
                end if
            end if

            ! regular extension
            mpv = mp_ext_type(etype, length)
            successful = .true.
            select type(mpv)
            type is (mp_value_type)
            class is (mp_ext_type)
                mpv%values = buffer(byteadvance+1:byteadvance+length)
                byteadvance = byteadvance + length
            class default
                successful = .false.
                deallocate(mpv)
                print *, "[Error: something went terribly wrong"
            end select
        end subroutine
end module
