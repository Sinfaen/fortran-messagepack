program packing
    use messagepack
    use iso_fortran_env
    implicit none

    ! variables to use
    class(mp_int_type), allocatable :: int_test
    class(mp_str_type), allocatable :: str_test
    class(mp_arr_type), allocatable :: arr_test
    class(mp_map_type), allocatable :: map_test

    byte, allocatable, dimension(:) :: buf
    character(:), allocatable :: small_text
    integer :: i
    integer :: length
    logical :: errored

    int_test = mp_int_type(4)

    print *, "Packing test"

    ! positive fix int test
    call pack_alloc(int_test, buf, errored)
    if (errored) then
        print *, "[Error: failed to pack int"
        stop 1
    end if
    ! we expect a single byte that contains the value 4
    if (size(buf) == 1) then
        if (buf(1) /= 4_int8) then
            print *, "[Error: failed to pack NFI. byte(1): ", buf(1)
            stop 1
        end if
    else
        print *, "[Error: failed to pack NFI. Size: ", size(buf)
        stop 1
    end if
    deallocate(buf)
    deallocate(int_test)
    print *, "[Info: PFI packing test succeeded"

    ! negative fix int test
    int_test = mp_int_type(-5)
    call pack_alloc(int_test, buf, errored)
    if (errored) then
        print *, "[Error: failed to pack NFI"
        stop 1
    end if
    ! we expect a single byte that contains the value -27 (0b11100101 as int8)
    if (size(buf) == 1) then
        if (buf(1) /= -27_int8) then
            print *, "[Error: failed to pack NFI. byte(1): ", buf(1)
            stop 1
        end if
    else
        print *, "[Error: failed to pack PFI. Size: ", size(buf)
    end if
    deallocate(buf)
    deallocate(int_test)
    print *, "[Info: NFI packing test succeeded"

    ! Str8 test
    allocate(character(len=163) :: small_text)
    small_text = "It's a dangerous business, Frodo, going "
    small_text = small_text // "out your door. You step onto the road, a"
    small_text = small_text // "nd if you don't keep your feet, there's "
    small_text = small_text // "no knowing where you might be swept off to."
    str_test = mp_str_type(small_text)
    call pack_alloc(str_test, buf, errored)
    if (errored) then
        print *, "[Error: failed to pack Str8"
        stop 1
    end if
    length = len(small_text)
    ! expect 163 + 2 bytes
    if (size(buf) == length + 2) then
        if (buf(1) /= MP_S8) then
            print *, "[Error: failed to pack str8. byte(1): ", buf(1)
            stop 1
        end if
    else
        print *, "[Error: failed to pack str8. Size: ", size(buf)
        stop 1
    end if
    ! expect reported number of bytes to be -93 (163 as int8)
    if (buf(2) /= -93) then
        print *, "[Error: reported number of bytes is not 163"
        stop 1
    end if
    ! compare text
    do i = 1,length
        if (buf(2+i) /= transfer(small_text(i:i), 1_int8)) then
            print *, "[Error: Str8 buffer did not match at index: ", 2+i
            print *, "[Info: ", transfer(small_text(i:i), 0_int8), "!=", buf(2+i)
            stop 1
        end if
    end do
    deallocate(buf)
    deallocate(str_test)
    deallocate(small_text)
    print *, "[Info: Str8 packing test succeeded"

    ! fixarray test
    arr_test = mp_arr_type(4_int64)
    arr_test%value(1)%obj = new_real32(32.1)
    arr_test%value(2)%obj = new_real64(-2.03_8)
    arr_test%value(3)%obj = mp_str_type("mochi")
    arr_test%value(4)%obj = mp_bool_type(.true.)
    call pack_alloc(arr_test, buf, errored)
    if (errored) then
        print *, "[Error: failed to pack fixarray"
        stop 1
    end if
    ! expect 22 bytes
    if (size(buf) == 22) then
        if (buf(1) /= ior(MP_FA_L, 4)) then
            print *, "[Error: failed to pack fixarray. byte(1): ", buf(1), "expected: ", ior(MP_FA_L, 4)
            stop 1
        end if
    else
        print *, "[Error: failed to pack fixarray correctly. Size: ", size(buf)
        stop 1
    end if
    deallocate(buf)
    deallocate(arr_test)
    print *, "[Info: Fixarray packing test succeeded"

    ! fixmap test
    arr_test = mp_arr_type(1_int64)
    arr_test%value(1)%obj = mp_str_type("a")

    map_test = mp_map_type(3_int64)
    map_test%keys(1)%obj   = new_real32(21.1)
    map_test%values(1)%obj = mp_bool_type(.false.)
    map_test%keys(2)%obj   = new_real64(74.1_real64)
    map_test%values(2)%obj = mp_nil_type()
    map_test%keys(3)%obj   = mp_int_type(3)
    map_test%values(3)%obj = arr_test
    call pack_alloc(map_test, buf, errored)
    if (errored) then
        print *, "[Error: failed to pack fixmap"
        stop 1
    end if
    ! expect 21 bytes
    if (size(buf) == 21) then
        if (buf(1) /= ior(MP_FM_L, 3)) then
            print *, "[Error: failed to pack fixmap. byte(1): ", buf(1), "expected: ", ior(MP_FM_L, 3)
            stop 1
        end if
    else
        print *, "[Error: failed to pack fixmap correctly. Size: ", size(buf)
        stop 1
    end if
    deallocate(buf)
    deallocate(map_test)
    print *, "[Info: Fixmap packing test succeeded"

end program
