! defines a class that stores callbacks for handling
! user extensions
module messagepack_user
    use iso_fortran_env
    use, intrinsic :: ieee_arithmetic
    use messagepack_value
    use byte_utilities

    implicit none

    private

    public :: mp_settings, unpack_func, unpack_callback
    public :: mp_timestamp_type, register_extension

    integer, parameter, public :: MP_TS_EXT = -1
    
    abstract interface
        subroutine unpack_func(buffer, byteadvance, is_little_endian, mpv, successful)
            import int64, mp_value_type
            byte, dimension(:), intent(in) :: buffer
            integer(kind=int64), intent(inout) :: byteadvance
            logical, intent(in) :: is_little_endian
            class(mp_value_type), allocatable, intent(out) :: mpv
            logical, intent(out) :: successful
        end subroutine
    end interface

    type :: unpack_callback
        procedure(unpack_func), pointer, nopass :: cb => null()
    end type

    ! defines settings used for serialization & deserialization with
    ! this library.
    ! - stores 
    type :: mp_settings
        class(unpack_callback), allocatable, dimension(:) :: f1
        class(unpack_callback), allocatable, dimension(:) :: f2
        class(unpack_callback), allocatable, dimension(:) :: f4
        class(unpack_callback), allocatable, dimension(:) :: f8
        class(unpack_callback), allocatable, dimension(:) :: f16
        class(unpack_callback), allocatable, dimension(:) :: e8
        class(unpack_callback), allocatable, dimension(:) :: e16
        class(unpack_callback), allocatable, dimension(:) :: e32
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
        integer(kind=int64) :: nanoseconds ! this must be positive
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
            logical :: err
            procedure(unpack_func), pointer :: p
            allocate(new_settings%f1(256))
            allocate(new_settings%f2(256))
            allocate(new_settings%f4(256))
            allocate(new_settings%f8(256))
            allocate(new_settings%f16(256))
            allocate(new_settings%e8(256))
            allocate(new_settings%e16(256))
            allocate(new_settings%e32(256))

            ! add timestamp here
            p => unpack_timestamp_32
            call new_settings%register_extension_super(MP_FE4, -1_int8, p, err)
            p => unpack_timestamp_64
            call new_settings%register_extension_super(MP_FE8, -1_int8, p, err)
            p => unpack_timestamp_96
            call new_settings%register_extension_super(MP_FE8, -1_int8, p, err)
        end function

        type(mp_timestamp_type) function new_timestamp(sec, ns)
            integer(kind=int64) :: sec
            integer(kind=int64) :: ns
            new_timestamp%seconds     = sec
            new_timestamp%nanoseconds = ns
        end function

        subroutine register_extension(this, ext, typeid, cb, error)
            ! Registers callbacks for handling extensions
            ! Only allows registering ids [0 127]
            class(mp_settings) :: this
            integer, intent(in) :: ext
            integer(kind=int8), intent(in) :: typeid
            procedure(unpack_func), pointer, intent(in) :: cb
            logical, intent(out) :: error

            if (typeid < 0) then
                error = .true.
                return
            end if
            call this%register_extension_super(ext, typeid, cb, error)
        end subroutine

        subroutine register_extension_super(this, ext, typeid, cb, error)
            ! Registers callbacks for handling extensions
            ! allows ids [-128 127]
            class(mp_settings) :: this
            integer, intent(in) :: ext
            integer(kind=int8), intent(in) :: typeid
            procedure(unpack_func), pointer, intent(in) :: cb
            logical, intent(out) :: error

            integer :: arr_index

            arr_index = typeid + 128 ! [-128, 127] -> [1, 256]

            select case(ext)
            case (MP_FE1)
                this%f1(arr_index)%cb => cb
            case (MP_FE2)
                this%f2(arr_index)%cb => cb
            case (MP_FE4)
                this%f4(arr_index)%cb => cb
            case (MP_FE8)
                this%f8(arr_index)%cb => cb
            case (MP_FE16)
                this%f16(arr_index)%cb => cb
            case (MP_E8)
                this%e8(arr_index)%cb => cb
            case (MP_E16)
                this%e16(arr_index)%cb => cb
            case (MP_E32)
                this%e32(arr_index)%cb => cb
            end select

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
                temp = this%seconds
                call mvbits(this%seconds, 0, 34, temp, 0)
                call mvbits(this%nanoseconds, 0, 30, temp, 34)
                call int_to_bytes_be_8(buf(3:10), temp)
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

        subroutine unpack_timestamp_64(buffer, byteadvance, is_little_endian, mpv, successful)
            byte, dimension(:), intent(in) :: buffer
            integer(kind=int64), intent(inout) :: byteadvance
            logical, intent(in) :: is_little_endian
            class(mp_value_type), allocatable, intent(out) :: mpv
            logical, intent(out) :: successful

            integer(kind=int64) :: temp, temp1, temp2

            if (size(buffer(byteadvance:)) < 10) then
                successful = .false.
                return
            end if
            
            byteadvance = byteadvance + 2
            temp = bytes_be_to_int_8(buffer(byteadvance:byteadvance+7), is_little_endian)
            call mvbits(temp, 0, 34, temp1, 0)
            call mvbits(temp, 34, 30, temp2, 0)
            mpv = mp_timestamp_type(temp1, temp2)
            byteadvance = byteadvance + 7

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
