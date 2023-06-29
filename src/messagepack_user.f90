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
    public :: mp_timestamp_type, is_timestamp, get_timestamp_ref, register_extension

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
        logical, dimension(256) :: f1_allocated
        logical, dimension(256) :: f2_allocated
        logical, dimension(256) :: f4_allocated
        logical, dimension(256) :: f8_allocated
        logical, dimension(256) :: f16_allocated
        logical, dimension(256) :: e8_allocated
        logical, dimension(256) :: e16_allocated
        logical, dimension(256) :: e32_allocated

        logical :: is_little_endian
    contains
        procedure :: register_extension
        procedure :: register_extension_super
        procedure :: print_value
        procedure :: print_value_with_args
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
            integer :: i
            allocate(new_settings%f1(256))
            allocate(new_settings%f2(256))
            allocate(new_settings%f4(256))
            allocate(new_settings%f8(256))
            allocate(new_settings%f16(256))
            allocate(new_settings%e8(256))
            allocate(new_settings%e16(256))
            allocate(new_settings%e32(256))
            do i = 1,256
                new_settings%f1_allocated = .false.
                new_settings%f2_allocated = .false.
                new_settings%f4_allocated = .false.
                new_settings%f8_allocated = .false.
                new_settings%f16_allocated = .false.
                new_settings%e8_allocated = .false.
                new_settings%e16_allocated = .false.
                new_settings%e32_allocated = .false.
            end do

            ! AFAIK there is no stdlib equivalent of C++20 std::endian
            new_settings%is_little_endian = detect_little_endian()

            ! add timestamp here
            p => unpack_timestamp_32
            call new_settings%register_extension_super(MP_FE4, -1_int8, p, err)
            p => unpack_timestamp_64
            call new_settings%register_extension_super(MP_FE8, -1_int8, p, err)
            p => unpack_timestamp_96
            call new_settings%register_extension_super(MP_E8, -1_int8, p, err)
        end function

        type(mp_timestamp_type) function new_timestamp(sec, ns)
            integer(kind=int64) :: sec
            integer(kind=int64) :: ns
            new_timestamp%seconds     = sec
            new_timestamp%nanoseconds = abs(ns)
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

            arr_index = typeid + 129 ! [-128, 127] -> [1, 256]

            select case(ext)
            case (MP_FE1)
                this%f1(arr_index)%cb => cb
                this%f1_allocated(arr_index) = .true.
            case (MP_FE2)
                this%f2(arr_index)%cb => cb
                this%f2_allocated(arr_index) = .true.
            case (MP_FE4)
                this%f4(arr_index)%cb => cb
                this%f4_allocated(arr_index) = .true.
            case (MP_FE8)
                this%f8(arr_index)%cb => cb
                this%f8_allocated(arr_index) = .true.
            case (MP_FE16)
                this%f16(arr_index)%cb => cb
                this%f16_allocated(arr_index) = .true.
            case (MP_E8)
                this%e8(arr_index)%cb => cb
                this%e8_allocated(arr_index) = .true.
            case (MP_E16)
                this%e16(arr_index)%cb => cb
                this%e16_allocated(arr_index) = .true.
            case (MP_E32)
                this%e32(arr_index)%cb => cb
                this%e16_allocated(arr_index) = .true.
            end select

            error = .false.
        end subroutine

        subroutine print_value(this, obj)
            ! Prints MessagePack object with default options
            ! @param[in] this - instance
            ! @param[in] obj - MessagePack object to print
            class(mp_settings) :: this
            class(mp_value_type), intent(in) :: obj
            call this%print_value_with_args(obj, 0, .false., -1)
        end subroutine

        recursive subroutine print_value_with_args(this, obj, indentation, &
                sameline, maxelems)
            ! Prints MessagePack object with a variety of configurability
            ! @param[in] this - instance
            ! @param[in] obj - MessagePack object to print in a pretty fashion
            ! @param[in] indentation - number of levels of indentation to print with
            ! @param[in] sameline - if true, compacts the output
            ! @param[in] maxelems - if non-negative, limits number of elements printed
            ! @returns None
            class(mp_settings), intent(in) :: this
            class(mp_value_type), intent(in) :: obj
            integer, intent(in) :: indentation
            logical, intent(in) :: sameline
            integer, intent(in) :: maxelems
            integer(kind=int64) :: i, j, ind

            if (.not. sameline) then
                do i = 1,indentation
                    write(*, "(A2)", advance="no") "  "
                end do
            end if
    
            select type(obj)
            type is (mp_value_type)
            class is (mp_nil_type)
                write(*, "(A)", advance="no") "nil"
            class is (mp_bool_type)
                if (obj%value) then
                    write(*, "(A)", advance="no") "true"
                else
                    write(*, "(A)", advance="no") "false"
                end if
            class is (mp_int_type)
                if (obj%unsigned_64) then
                    write(*, "(I0, A)", advance="no") obj%value, "[OUT-OF-RANGE]"
                else
                    write(*, "(I0)", advance="no") obj%value
                end if
                
            class is (mp_float_type)
                if (obj%is_64) then
                    write(*, "(F0.0)", advance="no") obj%f64value
                else
                    write(*, "(F0.0)", advance="no") obj%f32value
                end if
            class is (mp_str_type)
                write(*, "(A, A, A)", advance="no") char(34), obj%value, char(34)
            class is (mp_arr_type)
                write(*, "(A)", advance="no") "["
                printarr : do j = 1,obj%numelements()
                    call this%print_value_with_args(obj%value(j)%obj, 0, .true., maxelems)
                    write(*, "(A)", advance="no") ", "
                    if (maxelems > 0 .and. j > maxelems) then
                        write(*, "(A3)") "..."
                        exit printarr
                    end if
                end do printarr
                write(*, "(A)", advance="no") "]" 
            class is (mp_map_type)
                write(*, "(A)") "{"
                printmap : do j = 1, obj%numelements()
                    do i = 1,indentation+1
                        write(*, "(A2)", advance="no") "  "
                    end do
                    call this%print_value_with_args(obj%keys(j)%obj, indentation + 1, &
                        .true., maxelems)
                    write(*, "(A)", advance="no") " => "
                    call this%print_value_with_args(obj%values(j)%obj, indentation + 1, &
                        .true., maxelems)
                    print *, ","
                    if (maxelems > 0 .and. i > maxelems) then
                        write(*, "(A3)") "..."
                        exit printmap
                    end if
                end do printmap
                if (.not. sameline) then
                    do i = 1,indentation
                        write(*, "(A2)", advance="no") "  "
                    end do
                end if
                write(*, "(A)") "},"
            class is (mp_bin_type)
                write(*, "(A)", advance="no") "BIN["
                printbin : do j = 1,obj%numelements()
                    write(*, "(I0, A)", advance="no") obj%value(j), ", "
                    if (maxelems > 0 .and. j > maxelems) then
                        write(*, "(A)") "..."
                        exit printbin
                    end if
                end do printbin
                write(*, "(A)", advance="no") "]"
            class is (mp_ext_type)
                ind = obj%exttype + 129 ! TODO
                write(*, "(A)", advance="no") "EXT["
                printext : do j = 1,obj%numelements()
                    write(*, "(I0, A)", advance="no") obj%values(j), ", "
                    if (maxelems > 0 .and. j > maxelems) then
                        write(*, "(A)") "..."
                        exit printext
                    end if
                end do printext
                write(*, "(A)", advance="no") "]"
            end select
            if (.not. sameline) then
                print *, ""
            end if
        end subroutine

        subroutine get_size_timestamp(this, osize)
            class(mp_timestamp_type) :: this
            integer(kind=int64), intent(out) :: osize
            if (this%nanoseconds == 0 .and. this%seconds <= 4294967296_int64 .and. &
                    this%seconds >= 0) then
                osize = 6  ! timestamp32
            else if (this%nanoseconds  <=  1073741824_int64 &
                    .and. this%seconds <= 17179869184_int64 &
                    .and. this%seconds >= 0) then
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

            if (size(buffer(byteadvance+1:)) < 4) then
                successful = .false.
                return
            end if

            temp = bytes_be_to_int_4(buffer(byteadvance+1:byteadvance+4), is_little_endian)
            mpv = mp_timestamp_type(temp, 0)
            byteadvance = byteadvance + 4

            successful = .true.
        end subroutine

        subroutine unpack_timestamp_64(buffer, byteadvance, is_little_endian, mpv, successful)
            byte, dimension(:), intent(in) :: buffer
            integer(kind=int64), intent(inout) :: byteadvance
            logical, intent(in) :: is_little_endian
            class(mp_value_type), allocatable, intent(out) :: mpv
            logical, intent(out) :: successful

            integer(kind=int64) :: temp, temp1, temp2

            if (size(buffer(byteadvance+1:)) < 8) then
                successful = .false.
                return
            end if

            temp = bytes_be_to_int_8(buffer(byteadvance+1:byteadvance+8), is_little_endian)
            temp1 = 0
            temp2 = 0
            call mvbits(temp, 0, 34, temp1, 0)
            call mvbits(temp, 34, 30, temp2, 0)
            mpv = mp_timestamp_type(temp1, temp2)
            byteadvance = byteadvance + 8

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

            if (size(buffer(byteadvance+1:)) < 12) then
                successful = .false.
                return
            end if

            temp = bytes_be_to_int_4(buffer(byteadvance+1:byteadvance+4), is_little_endian)
            byteadvance = byteadvance + 4
            temp2 = bytes_be_to_int_8(buffer(byteadvance+1:byteadvance+8), is_little_endian)
            mpv = mp_timestamp_type(temp2, temp)
            byteadvance = byteadvance + 8

            successful = .true.
        end subroutine

        function is_timestamp(obj) result(res)
            class(mp_value_type), intent(in) :: obj
            logical :: res

            select type(obj)
            type is(mp_value_type)
            class is (mp_timestamp_type)
                res = .true.
            class default
                res = .false.
            end select
        end function is_timestamp

        subroutine get_timestamp_ref(obj, val, stat)
            class(mp_value_type), intent(in) :: obj
            class(mp_timestamp_type), allocatable, intent(out) :: val
            logical, intent(out) :: stat

            select type(obj)
            type is (mp_value_type)
            class is (mp_timestamp_type)
                val = obj
                stat = .true.
            class default
                stat = .false.
            end select
        end subroutine
end module
