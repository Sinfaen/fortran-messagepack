! defines a class that stores callbacks for handling
! user extensions
module messagepack_user
    use iso_fortran_env
    use, intrinsic :: ieee_arithmetic
    use messagepack_value
    use byte_utilities

    implicit none

    integer, parameter, public :: MP_TS_EXT = -1

    private

    public :: mp_settings
    public :: mp_timestamp_type

    ! defines settings used for serialization & deserialization with
    ! this library.
    ! - stores 
    type :: mp_settings
        logical, dimension(256) :: is_registered ! is a type registered
    contains
        procedure :: register_extension
        procedure :: register_extension_super
    end type
    interface mp_settings
        procedure :: new_settings
    end interface
    
    ! #region messagepack defined extensions go here
    type, extends(mp_value_type) :: mp_timestamp_type
        integer(kind=int64) :: seconds
        integer(kind=int64) :: nanoseconds
    contains
        procedure :: getsize => get_size_timestamp
        procedure :: pack => pack_timestamp
    end type
    interface mp_timestamp_type
        procedure :: new_timestamp
    end interface
    ! #endregion

    contains
        type(mp_settings) function new_settings()
            integer :: i
            logical :: err
            do i = 1,256
                new_settings%is_registered(i) = .false.
            end do

            ! add timestamp here
            call new_settings%register_extension_super(-1_int8, err)
        end function

        type(mp_timestamp_type) function new_timestamp(sec, ns)
            integer(kind=int64) :: sec
            integer(kind=int64) :: ns
            new_timestamp%seconds     = sec
            new_timestamp%nanoseconds = ns
        end function

        subroutine register_extension(this, typeid, error)
            ! Registers callbacks for handling extensions
            ! Only allows registering ids [0 127]
            class(mp_settings) :: this
            integer(kind=int8), intent(in) :: typeid
            logical, intent(out) :: error

            if (typeid < 0) then
                error = .true.
                return
            end if
            call this%register_extension_super(typeid, error)
        end subroutine

        subroutine register_extension_super(this, typeid, error)
            ! Registers callbacks for handling extensions
            ! allows ids [-128 127]
            class(mp_settings) :: this
            integer(kind=int8), intent(in) :: typeid
            logical, intent(out) :: error
            integer :: arr_index

            arr_index = typeid + 128 ! [-128, 127] -> [1, 256]
            if (this%is_registered(arr_index)) then
                error = .true.
                return
            end if

            this%is_registered(arr_index) = .true.
            error = .false.
        end subroutine

        subroutine get_size_timestamp(this, osize)
            class(mp_timestamp_type) :: this
            integer(kind=int64), intent(out) :: osize
            if (this%nanoseconds == 0 .and. this%seconds <= 4294967296_int64) then
                osize = 6  ! timestamp32
            else if (this%nanoseconds <= 1073741824_int64 &
                .and. this%seconds <= 17179869184_int64) then
                ! nanoseconds fit into uint30, seconds fit into uint34
                osize = 10 ! timestamp32
            else
                osize = 15 ! timestamp96
            end if
        end subroutine

        subroutine pack_timestamp(this, buf, error)
            class(mp_timestamp_type) :: this
            byte, dimension(:) :: buf
            logical, intent(out) :: error

            integer(kind=int64) :: temp
            integer(kind=int64) :: replength
            call this%getsize(replength)
            if (replength > size(buf)) then
                error = .true.
                return
            end if

            select case (replength)
            case (6)  ! timestamp32
                buf(1) = MP_FE4
                buf(2) = MP_TS_EXT
                call int_to_bytes_be_4(buf(3:6), int(this%seconds, kind=int32))
            case (10) ! timestamp64
                buf(1) = MP_FE8
                buf(2) = MP_TS_EXT
                temp = 0_int64
                !call mvbits(this%)
                !call mvbits
            case (15) ! timestamp96
                buf(1) = MP_E8
                buf(2) = 12
                buf(3) = MP_TS_EXT
                call int_to_bytes_be_4(buf(4:7), int(this%nanoseconds, kind=int32))
                call int_to_bytes_be_8(buf(8:15), this%seconds)
            end select

            error = .false.
        end subroutine

        subroutine unpack_timestamp_32(buffer, byteadvance, is_little_endian, mpv, successful)
            byte, dimension(:), intent(in) :: buffer
            integer(kind=int64), intent(inout) :: byteadvance
            logical, intent(in) :: is_little_endian
            class(mp_value_type), allocatable, intent(out) :: mpv
            logical, intent(out) :: successful

            integer(kind=int32) :: temp

            if (size(buffer(byteadvance:)) < 6) then
                successful = .false.
                return
            end if

            byteadvance = byteadvance + 2
            temp = bytes_be_to_int_4(buffer(byteadvance:byteadvance+3), is_little_endian)
            mpv = mp_timestamp_type(temp, 0)
            byteadvance = byteadvance + 3

            successful = .true.
        end subroutine

        subroutine unpack_timestamp_96(buffer, byteadvance, is_little_endian, mpv, successful)
            byte, dimension(:), intent(in) :: buffer
            integer(kind=int64), intent(inout) :: byteadvance
            logical, intent(in) :: is_little_endian
            class(mp_value_type), allocatable, intent(out) :: mpv
            logical, intent(out) :: successful

            integer(kind=int32) :: temp
            integer(kind=int64) :: temp2

            if (size(buffer(byteadvance:)) < 15) then
                successful = .false.
                return
            end if
            if (buffer(byteadvance + 1) /= 12) then
                successful = .false.
                return
            end if

            byteadvance = byteadvance + 2
            temp = bytes_be_to_int_4(buffer(byteadvance:byteadvance+3), is_little_endian)
            byteadvance = byteadvance + 4
            temp2 = bytes_be_to_int_8(buffer(byteadvance:byteadvance+7), is_little_endian)
            mpv = mp_timestamp_type(temp2, temp)
            byteadvance = byteadvance + 7

            successful = .true.
        end subroutine
end module
