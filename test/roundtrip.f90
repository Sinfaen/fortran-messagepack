program roundtrip
    use messagepack
    use iso_fortran_env
    implicit none

    ! messagepack setup
    class(mp_settings), allocatable :: mp_s

    ! variables to use
    logical :: error
    byte, dimension(:), allocatable :: buffer
    class(mp_map_type), allocatable :: mp_map
    class(mp_arr_type), allocatable :: mp_arr
    class(mp_bin_type), allocatable :: mp_bin
    class(mp_ext_type), allocatable :: mp_ext
    class(mp_timestamp_type), allocatable :: mp_ts
    class(mp_value_type), allocatable :: val

    class(mp_map_type), allocatable :: map_temp
    class(mp_arr_type), allocatable :: arr_temp
    class(mp_ext_type), allocatable :: ext_temp
    logical :: b_temp
    character(:), allocatable :: str_temp
    real(kind=real64), dimension(:), allocatable :: r_d_temp
    real(kind=real64) :: r_temp
    byte, dimension(:), allocatable :: byte_temp
    logical :: bool_temp
    integer :: i
    integer(kind=int64) :: i_temp

    print *, "Roundtrip test"
    mp_s = mp_settings()

    ! map test 1
    mp_arr = mp_arr_type(2_int64)
    mp_arr%value(1)%obj = new_real32(3.4)
    mp_arr%value(2)%obj = new_real32(7.5)

    mp_map = mp_map_type(3_int64)
    mp_map%keys(1)%obj = mp_int_type(4)
    mp_map%values(1)%obj = mp_str_type("hello world")
    mp_map%keys(2)%obj = mp_int_type(11)
    mp_map%values(2)%obj = mp_bool_type(.true.)
    mp_map%keys(3)%obj = mp_int_type(-20)
    mp_map%values(3)%obj = mp_arr

    ! serialize
    call pack_alloc(mp_map, buffer, error)
    if (error) then
        print *, "[Error: failed to pack map#1"
        stop 1
    end if

    ! deserialize
    call unpack_stream(mp_s, buffer, val, error)
    if (.not.(error)) then
        print *, "[Error: failed to unpack map#1"
        stop 1
    end if
    ! check equality
    call get_map_ref(val, map_temp, error)
    if (.not.(error)) then
        print *, "[Error: map#1 mark 1"
        stop 1
    end if
    if (map_temp%numelements() == 3) then
        call get_int(map_temp%keys(1)%obj, i_temp, error)
        if (i_temp /= 4 .or. (.not.(error))) then
            print *, "[Error: map#1:1k"
            stop 1
        end if
        call get_str(map_temp%values(1)%obj, str_temp, error)
        if (.not.(error)) then
            print *, "[Error: map#1:1v"
            stop 1
        end if
        call get_int(map_temp%keys(2)%obj, i_temp, error)
        if (i_temp /= 11 .or. (.not.(error))) then
            print *, "[Error: map#1:2k"
            stop 1
        end if
        call get_bool(map_temp%values(2)%obj, bool_temp, error)
        if (bool_temp .neqv. .true. .or. (.not.(error))) then
            print *, "[Error: map#1:2v"
            stop 1
        end if
        call get_int(map_temp%keys(3)%obj, i_temp, error)
        if (i_temp /= -20 .or. (.not.(error))) then
            print *, "[Error: map#1:3k"
            stop 1
        end if
        call unpack_array_real_1d(map_temp%values(3)%obj, r_d_temp, error)
        if (error) then
            print *, "[Error: map#1:3v"
            stop 1
        end if
        if (size(r_d_temp) /= 2 .or. r_d_temp(1) /= 3.4 .or. &
                r_d_temp(2) /= 7.5) then
            print *, "[Error: map#1:3ve"
            stop 1
        end if
    else
        print *, "[Error: map#1 has", map_temp%numelements(), "pairs"
    end if
    deallocate(buffer)
    deallocate(mp_arr)
    deallocate(mp_map)
    deallocate(val)
    print *, "[Info: map roundtrip test #1 succeeded"

    ! array test 1
    mp_arr = mp_arr_type(9_int64)
    mp_bin = mp_bin_type(300_int64)
    do i = 1,300
        mp_bin%value(i) = int(modulo(i, 55), kind=int8)
    end do
    mp_ext = mp_ext_type(25, 70000_int64)
    do i = 1,70000
        mp_ext%values(i) = int(modulo(i, 73), kind=int8)
    end do
    mp_arr%value(1)%obj = mp_str_type("foo")
    mp_arr%value(2)%obj = mp_nil_type()
    mp_arr%value(3)%obj = mp_timestamp_type(10, 300)
    mp_arr%value(4)%obj = mp_bin
    mp_arr%value(5)%obj = mp_bool_type(.false.)
    mp_arr%value(6)%obj = mp_ext
    mp_arr%value(7)%obj = mp_int_type(-2000000000_int64)
    mp_arr%value(8)%obj = new_real64(3.1415926535_real64)
    mp_arr%value(9)%obj = mp_bool_type(.true.)

    ! serialize
    call pack_alloc(mp_arr, buffer, error)
    if (error) then
        print *, "[Error: failed to pack arr#1"
        stop 1
    end if

    ! deserialize
    call unpack_stream(mp_s, buffer, val, error)
    if (.not.(error)) then
        print *, "[Error: failed to unpack arr#1"
        stop 1
    end if

    ! check equality
    call get_arr_ref(val, arr_temp, error)
    if (.not.(error)) then
        print *, "[Error: failed to get arr"
        stop 1
    end if
    if (arr_temp%numelements() /= 9) then
        print *, "[Error: incorrect array size"
        stop 1
    end if
    call get_str(arr_temp%value(1)%obj, str_temp, error)
    if (.not.(error) .or. str_temp /= "foo") then
        print *, "[Error: arr 1"
        stop 1
    end if
    if (.not.(is_nil(arr_temp%value(2)%obj))) then
        print *, "[Error: arr 2"
        stop 1
    end if
    call get_timestamp_ref(arr_temp%value(3)%obj, mp_ts, error)
    if (.not.(error) .or. mp_ts%seconds /= 10 .or. mp_ts%nanoseconds /= 300) then
        print *, "[Error: arr 3"
        stop 1
    end if
    call get_bin(arr_temp%value(4)%obj, byte_temp, error)
    if (error) then
        if (size(byte_temp) == 300) then
            do i = 1,300
                if (byte_temp(i) /= modulo(i, 55)) then
                    print *, "[Error: arr 4 bin val", i
                    stop 1
                end if
            end do
        else
            print *, "[Error: arr 4 bin length"
            stop 1
        end if
    else
        print *, "[Error: arr 4 not a bin"
        stop 1
    end if
    call get_bool(arr_temp%value(5)%obj, b_temp, error)
    if (.not.(error) .or. b_temp .neqv. .false.) then
        print *, "[Error: arr 5"
        stop 1
    end if
    call get_ext_ref(arr_temp%value(6)%obj, ext_temp, error)
    if (error .and. ext_temp%numelements() == 70000) then
        do i = 1,70000
            if (ext_temp%values(i) /= modulo(i, 73)) then
                print *, "[Error: arr 6 ext val", i
                stop 1
            end if
        end do
    else
        print *, "[Error: arr 6 ext inv"
        stop 1
    end if
    call get_int(arr_temp%value(7)%obj, i_temp, error)
    if (.not.(error) .or. i_temp /= -2000000000_int64) then
        print *, "[Error: arr 7"
        stop 1
    end if
    call get_real(arr_temp%value(8)%obj, r_temp, error)
    if (.not.(error) .or. r_temp /= 3.1415926535_real64) then
        print *, "[Error: arr 8"
        stop 1
    end if
    call get_bool(arr_temp%value(9)%obj, b_temp, error)
    if (.not.(error) .or. b_temp .neqv. .true.) then
        print *, "[Error: arr 9"
        stop 1
    end if
end program