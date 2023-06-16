program packing
    use messagepack
    use iso_fortran_env
    implicit none

    ! variables to use
    class(mp_int_type), allocatable :: int_test
    class(mp_str_type), allocatable :: str_test
    class(mp_arr_type), allocatable :: arr_test
    class(mp_map_type), allocatable :: map_test
    class(mp_ext_type), allocatable :: ext_test
    clasS(mp_timestamp_type), allocatable :: ts_test

    byte, allocatable, dimension(:) :: buf
    character(:), allocatable :: small_text
    integer :: i
    integer :: length
    logical :: errored

    byte, dimension(3) :: b_3_temp
    byte, dimension(4) :: b_4_temp
    byte, dimension(6) :: b_6_temp
    byte, dimension(10) :: b_10_temp
    byte, dimension(18) :: b_18_temp
    byte, dimension(:), allocatable :: b_var_temp

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

    ! fixext1 test
    ext_test = mp_ext_type(5, 1_int64)
    ext_test%values(1) = 71
    call pack_alloc(ext_test, buf, errored)
    if (errored) then
        print *, "[Error: failed to pack fixext1"
        stop 1
    end if
    ! expect 3 bytes
    b_3_temp = (/MP_FE1,5,71/)
    if (size(buf) == 3) then
        do i = 1,3
            if (buf(i) /= b_3_temp(i)) then
                print *, "[Error: packed fixext1 byte[", i, "] has value", buf(i), ". Expected:", b_3_temp(i)
                stop 1
            end if
        end do
    else
        print *, "[Error: failed to pack fixext1. Size: ", size(buf)
        stop 1
    end if

    deallocate(buf)
    deallocate(ext_test)
    print *, "[Info: Fixext1 packing test succeeded"

    ! fixext2 test
    ext_test = mp_ext_type(-2, 2_int64)
    ext_test%values(1) = 19
    ext_test%values(2) = 100
    call pack_alloc(ext_test, buf, errored)
    if (errored) then
        print *, "[Error: failed to pack fixext2"
        stop 1
    end if
    ! expect 4 bytes
    b_4_temp = (/MP_FE2,-2,19,100/)
    if (size(buf) == 4) then
        do i = 1,4
            if (buf(i) /= b_4_temp(i)) then
                print *, "[Error: packed fixext2 byte[", i, "] has value", buf(i), ". Expected:", b_4_temp(i)
                stop 1
            end if
        end do
    else
        print *, "[Error: failed to pack fixext2. Size: ", size(buf)
        stop 1
    end if

    deallocate(buf)
    deallocate(ext_test)
    print *, "[Info: Fixext2 packing test succeeded"

    ! fixext4 test
    ext_test = mp_ext_type(127, 4_int64)
    ext_test%values(1) = 0
    ext_test%values(2) = 1
    ext_test%values(3) = 2
    ext_test%values(4) = 3
    call pack_alloc(ext_test, buf, errored)
    if (errored) then
        print *, "[Error: failed to pack fixext4"
        stop 1
    end if
    ! expect 6 bytes
    b_6_temp = (/MP_FE4,127,0,1,2,3/)
    if (size(buf) == 6) then
        do i = 1,6
            if (buf(i) /= b_6_temp(i)) then
                print *, "[Error: packed fixext4 byte[", i, "] has value", buf(i), ". Expected:", b_6_temp(i)
                stop 1
            end if
        end do
    else
        print *, "[Error: failed to pack fixext4. Size: ", size(buf)
        stop 1
    end if

    deallocate(buf)
    deallocate(ext_test)
    print *, "[Info: Fixext4 packing test succeeded"

    ! fixext8 test
    ext_test = mp_ext_type(127, 8_int64)
    ext_test%values(1) = 0
    ext_test%values(2) = -1
    ext_test%values(3) = -2
    ext_test%values(4) = -3
    ext_test%values(5) = -4
    ext_test%values(6) = -5
    ext_test%values(7) = -6
    ext_test%values(8) = -7
    call pack_alloc(ext_test, buf, errored)
    if (errored) then
        print *, "[Error: failed to pack fixext8"
        stop 1
    end if
    ! expect 10 bytes
    b_10_temp = (/MP_FE8,127,0,-1,-2,-3,-4,-5,-6,-7/)
    if (size(buf) == 10) then
        do i = 1,10
            if (buf(i) /= b_10_temp(i)) then
                print *, "[Error: packed fixext8 byte[", i, "] has value", buf(i), ". Expected:", b_10_temp(i)
                stop 1
            end if
        end do
    else
        print *, "[Error: failed to pack fixext8. Size: ", size(buf)
        stop 1
    end if

    deallocate(buf)
    deallocate(ext_test)
    print *, "[Info: Fixext8 packing test succeeded"

    ! fixext16 test
    ext_test = mp_ext_type(-128, 16_int64)
    do i=1,16
        ext_test%values(i) = i*i - 10*i
    end do
    call pack_alloc(ext_test, buf, errored)
    if (errored) then
        print *, "[Error: failed to pack fixext16"
        stop 1
    end if
    ! expect 18 bytes
    b_18_temp = (/MP_FE16,-128,-9,-16,-21,-24,-25,-24,-21,-16,-9,0,11,24,39,56,75,96/)
    if (size(buf) == 18) then
        do i = 1,18
            if (buf(i) /= b_18_temp(i)) then
                print *, "[Error: packed fixext16 byte[", i, "] has value", buf(i), ". Expected:", b_18_temp(i)
                stop 1
            end if
        end do
    else
        print *, "[Error: failed to pack fixext16. Size: ", size(buf)
        stop 1
    end if

    deallocate(buf)
    deallocate(ext_test)
    print *, "[Info: Fixext16 packing test succeeded"

    ! ext8 test
    ext_test = mp_ext_type(5, 3_int64)
    ext_test%values(1) = 77
    ext_test%values(2) = 76
    ext_test%values(3) = 75
    call pack_alloc(ext_test, buf, errored)
    if (errored) then
        print *, "[Error: failed to pack ext8"
        stop 1
    end if
    ! expect 18 bytes
    b_6_temp = (/MP_E8,3,5,77,76,75/)
    if (size(buf) == 6) then
        do i = 1,6
            if (buf(i) /= b_6_temp(i)) then
                print *, "[Error: packed ext8 byte[", i, "] has value", buf(i), ". Expected:", b_6_temp(i)
                stop 1
            end if
        end do
    else
        print *, "[Error: failed to pack ext8. Size: ", size(buf), "Expected: 6"
        stop 1
    end if

    deallocate(buf)
    deallocate(ext_test)
    print *, "[Info: Ext8 packing test succeeded"

    ! ext16 test
    ext_test = mp_ext_type(5, 300_int64) ! 300 (0x012c)
    do i=1,300
        ext_test%values(i) = modulo(i, 13)
    end do
    call pack_alloc(ext_test, buf, errored)
    if (errored) then
        print *, "[Error: failed to pack ext16"
        stop 1
    end if
    ! expect 304 bytes
    allocate(b_var_temp(304))
    b_var_temp(1) = MP_E16
    b_var_temp(2) = 1  ! 0x01
    b_var_temp(3) = 44 ! 0x2c
    b_var_temp(4) = 5
    do i=1,300
        b_var_temp(4+i) = modulo(i, 13)
    end do
    if (size(buf) == 304) then
        do i = 1,304
            if (buf(i) /= b_var_temp(i)) then
                print *, "[Error: packed ext16 byte[", i, "] has value", buf(i), ". Expected:", b_var_temp(i)
                stop 1
            end if
        end do
    else
        print *, "[Error: failed to pack ext16. Size: ", size(buf), "Expected: 6"
        stop 1
    end if

    deallocate(buf)
    deallocate(ext_test)
    deallocate(b_var_temp)
    print *, "[Info: Ext16 packing test succeeded"

    ! ext32 test
    ext_test = mp_ext_type(100, 70000_int64) ! 70000 (0x00011170)
    do i=1,70000
        ext_test%values(i) = modulo(-i, 14)
    end do
    call pack_alloc(ext_test, buf, errored)
    if (errored) then
        print *, "[Error: failed to pack ext32"
        stop 1
    end if
    ! expect 70006 bytes
    allocate(b_var_temp(70006))
    b_var_temp(1) = MP_E32
    b_var_temp(2) = 0   ! 0x00
    b_var_temp(3) = 1   ! 0x01
    b_var_temp(4) = 17  ! 0x11
    b_var_temp(5) = 112 ! 0x70
    b_var_temp(6) = 100
    do i=1,70000
        b_var_temp(6+i) = modulo(-i, 14)
    end do
    if (size(buf) == 70006) then
        do i = 1,70006
            if (buf(i) /= b_var_temp(i)) then
                print *, "[Error: packed ext32 byte[", i, "] has value", buf(i), ". Expected:", b_var_temp(i)
                stop 1
            end if
        end do
    else
        print *, "[Error: failed to pack ext32. Size: ", size(buf), "Expected: 6"
        stop 1
    end if

    deallocate(buf)
    deallocate(ext_test)
    deallocate(b_var_temp)
    print *, "[Info: Ext32 packing test succeeded"

    ! timestamp32 test
    ts_test = mp_timestamp_type(50, 0)
    call pack_alloc(ts_test, buf, errored)
    if (errored) then
        print *, "[Error: failed to pack timestamp32"
        stop 1
    end if
    ! expect 6 bytes
    b_6_temp = (/MP_FE4,-1,0,0,0,50/)
    if (size(buf) == 6) then
        do i = 1,6
            if (buf(i) /= b_6_temp(i)) then
                print *, "[Error: packed timestamp32 byte[]", i, "] ==", buf(i), "Expected:", b_6_temp(i)
                stop 1
            end if
        end do
    else
        print *, "[Error: failed to pack timestamp32. Size:", size(buf), "Expected: 6"
        stop 1
    end if
    deallocate(ts_test)
    print *, "[Info: timestamp32 packing test succeeded"

    ! timestamp64 test
    ! 0x000003a0 000088b8
    ts_test = mp_timestamp_type(35000, 232)
    call pack_alloc(ts_test, buf, errored)
    if (errored) then
        print *, "[Error: failed to pack timestamp64"
        stop 1
    end if
    ! expect 10 bytes
    b_10_temp(1) = MP_FE8
    b_10_temp(2) = -1_int8
    b_10_temp(3) = 0
    b_10_temp(4) = 0
    b_10_temp(5) = 3
    b_10_temp(6) = -96 ! 0xa0
    b_10_temp(7) = 0
    b_10_temp(8) = 0
    b_10_temp(9) = -120 ! 0x88
    b_10_temp(10) = -72 ! 0xb8
    if (size(buf) == 10) then
        do i = 1,10
            if (buf(i) /= b_10_temp(i)) then
                print *, "[Error: packed timestamp64 byte[]", i, "] ==", buf(i), "Expected:", b_6_temp(i)
                stop 1
            end if
        end do
    else
        print *, "[Error: failed to pack timestamp64. Size:", size(buf), "Expected: 8"
        stop 1
    end if
    deallocate(buf)
    deallocate(ts_test)
    print *, "[Info: timestamp64 packing test succeeded"

    ! timestamp96 test
    ! 0x000004d2
    ! 0xfffffffffffff830
    ts_test = mp_timestamp_type(-2000, 1234)
    call pack_alloc(ts_test, buf, errored)
    if (errored) then
        print *, "[Error: failed to pack timestamp96"
        stop 1
    end if
    ! expect 15 bytes
    allocate(b_var_temp(15))
    b_var_temp(1) = MP_E8
    b_var_temp(2) = 12
    b_var_temp(3) = -1
    b_var_temp(4) = 0
    b_var_temp(5) = 0
    b_var_temp(6) = 4
    b_var_temp(7) = -46 ! 0xd2
    b_var_temp(8) = -1
    b_var_temp(9) = -1
    b_var_temp(10) = -1
    b_var_temp(11) = -1
    b_var_temp(12) = -1
    b_var_temp(13) = -1
    b_var_temp(14) = -8 ! 0xf8
    b_var_temp(15) = 48 ! 0x30
    if (size(buf) == 15) then
        do i = 1,15
            if (buf(i) /= b_var_temp(i)) then
                print *, "[Error: packed timestamp96 byte[]", i, "] ==", buf(i), "Expected:", b_6_temp(i)
                stop 1
            end if
        end do
    else
        print *, "[Error: failed to pack timestamp96. Size:", size(buf), "Expected: 15"
        stop 1
    end if
    deallocate(buf)
    deallocate(b_var_temp)
    deallocate(ts_test)
    print *, "[Info: timestamp96 packing test succeeded"

end program
