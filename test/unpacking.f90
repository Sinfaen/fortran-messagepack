program unpacking
    use messagepack
    implicit none

    ! variables to use
    class(mp_settings), allocatable :: mp_s
    byte, allocatable, dimension(:) :: stream ! buffer of bytes
    class(mp_value_type), allocatable :: mpv  ! pointer to value
    integer :: i, i_tmp
    logical :: btmp
    integer(kind=int64) :: itmp
    real(kind=real64) :: rtmp
    integer(kind=int64), dimension(3) :: i_a_3
    integer(kind=int64), dimension(2) :: i_a_2
    character(:), allocatable :: stmp
    byte, dimension(:), allocatable :: byte_tmp
    class(mp_arr_type), allocatable :: arrtmp
    class(mp_map_type), allocatable :: maptmp
    class(mp_ext_type), allocatable :: exttmp
    class(mp_timestamp_type), allocatable :: ts_tmp
    logical :: status

    print *, "Unpacking test"
    call print_endianness()
    mp_s = mp_settings()

    ! positive fix int test: VALUE = 45
    allocate(stream(1))
    stream(1) = 45
    call unpack_stream(mp_s, stream, mpv, status)
    deallocate(stream)
    if (.not.(status)) then
        print *, "[Error: issue occurred with unpacking stream (PFI)"
        stop 1
    end if
    if (.not.(is_int(mpv))) then
        print *, "[Error: Did not unpack an int"
        stop 1
    end if
    call get_int(mpv, itmp, status)
    if (itmp /= 45) then
        write(*,*) "[Error: Unpacked ", itmp, " instead of 45"
        stop 1
    end if
    deallocate(mpv)
    print *, "[Info: PFI test succeeded"

    ! negative fix int test: VALUE = -2
    allocate(stream(1))
    stream(1) = -30 ! 0b11100010 as int8
    call unpack_stream(mp_s, stream, mpv, status)
    deallocate(stream)
    if (.not.(status)) then
        print *, "[Error: issue occurred with unpacking stream (NFI)"
        stop 1
    end if
    if (.not.(is_int(mpv))) then
        print *, "[Error: Did not unpack an int"
        stop 1
    end if
    call get_int(mpv, itmp, status)
    if (itmp /= -2) then
        write(*,*) "[Error: Unpacked ", itmp, " instead of -2"
        stop 1
    end if
    deallocate(mpv)
    print *, "[Info: NFI test succeeded"

    ! test that a uint32 value of 3147483647 (0xbb9ac9ff) is properly unpacked
    allocate(stream(5))
    stream(1) = MP_U32
    stream(2) =  -69_int8 ! 0xbb
    stream(3) = -102_int8 ! 0x9a
    stream(4) =  -55_int8 ! 0xc9
    stream(5) =   -1_int8 ! 0xff
    call unpack_stream(mp_s, stream, mpv, status)
    deallocate(stream)
    if (.not.(status)) then
        print *, "[Error: issue occurred with unpacking stream(UInt32)"
        stop 1
    end if
    if (.not.(is_int(mpv))) then
        print *, "[Error: Did not unpack an int"
        stop 1
    end if
    call get_int(mpv, itmp, status)
    if (itmp /= 3147483647_int64) then
        write(*,*) "[Error: Unpacked ", itmp, " instead of 3147483647"
        stop 1
    end if
    deallocate(mpv)
    print *, "[Info: U32 test succeeded"

    ! test that a value between 2^63 - 1 && 2^64 - 1 is marked as an unsigned value
    ! VALUE = 2^63 + 1345 = 9223372036854777153 == 0x8000000000000541
    ! when interpreted as Int64, == -9223372036854774463
    allocate(stream(9))
    stream(1) = MP_U64
    stream(2) = -128 ! 0x80
    stream(3) =    0 ! 0x00
    stream(4) =    0 ! 0x00
    stream(5) =    0 ! 0x00
    stream(6) =    0 ! 0x00
    stream(7) =    0 ! 0x00
    stream(8) =    5 ! 0x05
    stream(9) =   65 ! 0x41
    call unpack_stream(mp_s, stream, mpv, status)
    deallocate(stream)
    if (.not.(status)) then
        print *, "[Error: issue occurred with unpacking stream(UInt64 big)"
        stop 1
    end if
    if (.not.(is_int(mpv))) then
        print *, "[Error: Did not unpack an int"
        stop 1
    end if
    call get_int(mpv, itmp, status)
    if (itmp /= -9223372036854774463_int64) then
        write(*,*) "[Error: Unpacked (", itmp, ") instead of reinterpreted (-9223372036854774463)"
        stop 1
    end if
    if (.not. is_unsigned(mpv)) then
        print *, "[Error: int was not marked as unsigned"
    end if
    deallocate(mpv)
    print *, "[Info: U64 test succeeded"

    ! fixstr test: "Lego"
    allocate(stream(5))
    stream(1) = ior(MP_FS_L, 4)
    stream(2) = 76  ! L
    stream(3) = 101 ! e
    stream(4) = 103 ! g
    stream(5) = 111 ! o
    call unpack_stream(mp_s, stream, mpv, status)
    deallocate(stream)
    if (.not.(status)) then
        print *, "[Error: issue occurred with unpacking stream(fixstr)"
        stop 1
    end if
    if (.not.(is_str(mpv))) then
        print *, "[Error: Did not unpack an int"
        stop 1
    end if
    call get_str(mpv, stmp, status)
    if (stmp /= "Lego") then
        print *, "[Error: fixstr unpacked '", stmp, "' instead of 'Lego'"
        stop 1
    end if
    print *, "[Info: Fixstr test succeeded"

    ! fixarray test: 3 elements
    ! - positive fix int = 12
    ! - negative fix int = -3
    ! - int 16 =  32,000 0x7d00
    ! 1 + 1 + 1 + 3 = 6 bytes
    allocate(stream(6))
    stream(1) = ior(MP_FA_L, 3) ! fixarray byte mark
    stream(2) = 12  ! positive fix int
    stream(3) = -29 ! negative fix int: 0b11100011 as int8
    stream(4) = MP_I16 ! int 16 byte mark
    stream(5) = 125 ! 0x7d
    stream(6) = 0   ! 0x00
    call unpack_stream(mp_s, stream, mpv, status)
    deallocate(stream)
    if (.not.(status)) then
        print *, "[Error: issue occurred with unpacking stream(fixarr)"
        stop 1
    end if
    ! check length of the array
    i_a_3 = (/12, -3, 32000/)
    if (mpv%numelements() /= 3) then
        print *, "[Error: unpacked fixarray contains ", mpv%numelements(), &
            " elements instead of 3"
        stop 1
    end if
    call get_arr_ref(mpv, arrtmp, status)
    if (.not.(status)) then
        print *, "[Error: did not unpack mp_arr_type"
        stop 1
    end if

    ! loop over all elements
    do i = 1,3
        if (.not. is_int(arrtmp%value(i)%obj)) then
            print *, "[Error: fixarray[", i, "] is not an int"
            stop 1
        end if
        call get_int(arrtmp%value(i)%obj, itmp, status)
        if (itmp /= i_a_3(i)) then
            print *, "[Error: unpacked ", itmp, "instead of", i_a_3(i), "for fixarray > ", i
        end if
    end do
    deallocate(mpv)
    print *, "[Info: Fixarray test succeeded"

    ! array16 test: 17 elements
    ! 8 bool all false, 8 bool all true
    ! 1 nil
    ! 3 + 17
    allocate(stream(20))
    stream(1) = MP_A16
    stream(2) = 0
    stream(3) = 17
    do i = 1,8
        stream(3 + i)  = MP_F
        stream(11 + i) = MP_T
    end do
    stream(20) = MP_NIL
    call unpack_stream(mp_s, stream, mpv, status)
    deallocate(stream)
    if (.not.(status)) then
        print *, "[Error: unpacking stream(array16)"
        stop 1
    end if
    ! check length
    call get_arr_ref(mpv, arrtmp, status)
    if (.not. status) then
        print *, "[Error: failed to unpack array16"
        stop 1
    end if
    if (arrtmp%numelements() /= 17) then
        print *, "[Error: unpacked array16 contains ", arrtmp%numelements(), &
            " elements instead of 20"
        stop 1
    end if
    ! check elements
    do i = 1,16
        call get_bool(arrtmp%value(i)%obj, btmp, status)
        if (.not.(status)) then
            print *, "[Error: array16", i, "is not a boolean"
            stop 1
        end if
        if (btmp .neqv. (i > 8)) then
            print *, "[Error: array16", i, "is not the expected value", i > 8
            stop 1
        end if
    end do
    if (.not.(is_nil(arrtmp%value(17)%obj))) then
        print *, "[Error: array16(17) is not nil"
        stop 1
    end if
    deallocate(mpv)
    print *, "[Info: array16 test succeeded"

    ! array32 test: 2^20 elements
    ! all bin8 values of one byte each, modulo(i, 27)
    ! 5 + 1048576 * 3
    allocate(stream(3145733))
    stream(1) = MP_A32
    stream(2:5) = int((/0,16,0,0/), kind=int8)
    do i = 1,1048576
        stream(6+3*(i-1)) = MP_B8
        stream(7+3*(i-1)) = 1
        stream(8+3*(i-1)) = int(modulo(i, 27), kind=int8)
    end do
    call unpack_stream(mp_s, stream, mpv, status)
    deallocate(stream)
    if (.not.(status)) then
        print *, "[Error: unpacking stream(array32)"
        stop 1
    end if
    ! check length
    call get_arr_ref(mpv, arrtmp, status)
    if (.not. status) then
        print *, "[Error: failed to unpack array32"
        stop 1
    end if
    if (arrtmp%numelements() /= 1048576) then
        print *, "[Error: unpacked array32 contains ", arrtmp%numelements(), &
            " elements instead of 20"
        stop 1
    end if
    ! check elements
    do i = 1,1048576
        call get_bin(arrtmp%value(i)%obj, byte_tmp, status)
        if (.not.(status)) then
            print *, "[Error: unpacked array32", i, "is not a bin"
            stop 1
        end if
        if (size(byte_tmp) /= 1 .or. byte_tmp(1) /= modulo(i, 27)) then
            print *, "[Error: unpacked array32", i, "is invalid"
            stop 1
        end if
    end do
    deallocate(mpv)
    print *, "[Info: array32 test succeeded"

    ! fixmap test: 3 elements
    ! also serves as a double container test & float32 test
    ! - [fixstr - "reptile"] = [fixstr - "mochi"] 8 + 6 bytes
    ! - [int 32 - 2000000000] = [float - 3.5] 5 + 5 bytes
    ! (0x77359400, 0x40600000)
    ! - [bool - true] = [fixarray 2 elements
    !   - pos fixint = [4]  1 byte
    !   - neg fixint = [-2] 1 byte
    ! 1 + 14 + 10 + 1 + 1 + 1 + 1 = 29 bytes
    allocate(stream(29))
    stream(1)  = ior(MP_FM_L, 3) ! fixmap byte mark
    stream(2)  = ior(MP_FS_L, 7)  ! fixstr byte mark
    stream(3)  = 114 ! r
    stream(4)  = 101 ! e
    stream(5)  = 112 ! p
    stream(6)  = 116 ! t
    stream(7)  = 105 ! i
    stream(8)  = 108 ! l
    stream(9)  = 101 ! e
    stream(10) = ior(MP_FS_L, 5) ! fixstr byte mark
    stream(11) = 109 ! m
    stream(12) = 111 ! o
    stream(13) = 99  ! c
    stream(14) = 104 ! h
    stream(15) = 105 ! i
    stream(16) = MP_I32
    stream(17) = 119  ! 0x77
    stream(18) = 53   ! 0x35
    stream(19) = -108 ! 0x94
    stream(20) = 0    ! 0x00
    stream(21) = MP_F32
    stream(22) = 64 ! 0x40
    stream(23) = 96 ! 0x60
    stream(24) = 0  ! 0x00
    stream(25) = 0  ! 0x00
    stream(26) = MP_T
    stream(27) = ior(MP_FA_L, 2) ! fixarr byte mark
    stream(28) =   4 ! positive fix int
    stream(29) = -30 ! 0b11100010 as int8
    call unpack_stream(mp_s, stream, mpv, status)
    deallocate(stream)
    if (.not.(status)) then
        print *, "[Error: issue occurred with unpacking stream(fixmap)"
        stop 1
    end if
    if (mpv%numelements() /= 3) then
        print *, "[Error: unpacked fixmap contains ", mpv%numelements(), " elements instead of 3"
        stop 1
    end if

    call get_map_ref(mpv, maptmp, status)
    if (.not. status) then
        print *, "[Error: did not unpack mp_map_type"
        stop 1
    end if

    ! check ["reptile"] == "mochi"
    if (.not. is_str(maptmp%keys(1)%obj)) then
        print *, "[Error: fixmap[1] key is not a str"
        stop 1
    end if
    call get_str(maptmp%keys(1)%obj, stmp, status)
    if (stmp /= "reptile") then
        print *, "[Error: fixmap[1] key is", stmp, " not 'reptile'"
        stop 1
    end if
    if (.not. is_str(maptmp%values(1)%obj)) then
        print *, "[Error: fixmap[1] value is not a str"
        stop 1
    end if
    call get_str(maptmp%values(1)%obj, stmp, status)
    if (stmp /= "mochi") then
        print *, "[Error: fixmap[1] value is", stmp, " not 'mochi'"
        stop 1
    end if
    ! check [3000000000] == 3.5
    if (.not. is_int(maptmp%keys(2)%obj)) then
        print *, "[Error: fixmap[2] key is not an int"
        stop 1
    end if
    call get_int(maptmp%keys(2)%obj, itmp, status)
    if (itmp /= 2000000000) then
        print *, "[Error: fixmap[2] key is", itmp, "not 2000000000"
        stop 1
    end if
    if (.not. is_float(maptmp%values(2)%obj)) then
        print *, "[Error: fixmap[2] key is not a float"
        stop 1
    end if
    call get_real(maptmp%values(2)%obj, rtmp, status)
    if (rtmp /= 3.5) then
        print *, "[Error: fixmap[2] value is", rtmp, "not 3.5"
        stop 1
    end if
    ! check [true] == [4, -31]
    if (.not. is_bool(maptmp%keys(3)%obj)) then
        print *, "[Error: fixmap[3] key is not a bool"
        stop 1
    end if
    call get_bool(maptmp%keys(3)%obj, btmp, status)
    if (btmp .neqv. .true.) then
        print *, "[Error: fixmap[3] key is not .true."
        stop 1
    end if
    if (.not. is_arr(maptmp%values(3)%obj)) then
        print *, "[Error: fixmap[3] value is not an array"
        stop 1
    end if
    call get_arr_ref(maptmp%values(3)%obj, arrtmp, status)
    i_a_2 = (/4, -2/)
    do i = 1,2
        if (.not. is_int(arrtmp%value(i)%obj)) then
            print *, "[Error: fixmap[3] value[", i, "] is not an int"
            stop 1
        end if
        call get_int(arrtmp%value(i)%obj, itmp, status)
        if (itmp /= i_a_2(i)) then
            print *, "[Error: fixmap[3] value[", i, "] was not ", i_a_2(i)
            stop 1
        end if
    end do
    deallocate(mpv)
    print *, "[Info: Fixmap test succeeded"

    ! map16 test: 2^13 elements
    ! keys: NFI -1:-10
    ! values: fixext1: 2, 1:10
    ! 3 + 8192 * 1 + 8192 * 3
    allocate(stream(32771))
    stream(1) = MP_M16
    stream(2) = 32
    stream(3) = 0
    do i = 1,8192
        ! key
        stream(4+4*(i-1)) = int(-32 + modulo(i, 11), kind=int8)
        ! value
        stream(5+4*(i-1)) = MP_FE1
        stream(6+4*(i-1)) = 2
        stream(7+4*(i-1)) = int(modulo(i, 11), kind=int8)
    end do
    call unpack_stream(mp_s, stream, mpv, status)
    deallocate(stream)
    if (.not.(status)) then
        print *, "[Error: issue occurred with unpacking stream(map16)"
        stop 1
    end if
    if (mpv%numelements() /= 8192) then
        print *, "[Error: unpacked map16 contains ", mpv%numelements(), &
            " elements instead of 8192"
        stop 1
    end if
    ! check elements
    call get_map_ref(mpv, maptmp, status)
    if (.not.(status)) then
        print *, "[Error: did not unpack mp_map_type"
        stop 1
    end if
    do i = 1,8192
        ! check key
        call get_int(maptmp%keys(i)%obj, itmp, status)
        if (.not.(status)) then
            print *, "[Error: map16 key", i, "is not an int"
            stop 1
        end if
        if (itmp /= -(modulo(i, 11))) then
            print *, "[Error: map16 key", i, "is not", -(modulo(i, 11))
            stop 1
        end if
        ! check value
        call get_ext_ref(maptmp%values(i)%obj, exttmp, status)
        if (.not.(status)) then
            print *, "[Error: map16 value", i, "is not an ext"
            stop 1
        end if
        if (exttmp%numelements() /= 1 .or. &
                exttmp%values(1) /= modulo(i, 11)) then
            print *, "[Error: map16 value", i, "has value", exttmp%values(1), &
                "instead of", modulo(i, 11)
            stop 1
        end if
    end do
    deallocate(mpv)
    print *, "[Info: map16 test succeeded"

    ! map32 test: 2^18 elements
    ! keys: PFI 22:37
    ! values: fixext2 3, 1..5, 1..13
    ! 5 + 262144*1 + 262144*4
    allocate(stream(1310725))
    stream(1) = MP_M32
    stream(2:5) = int((/0,4,0,0/), kind=int8)
    do i = 1,262144
        ! key
        stream(6+5*(i-1)) = int(21 + modulo(i, 16), kind=int8)
        ! value
        stream(7 +5*(i-1)) = MP_FE2
        stream(8 +5*(i-1)) = 3
        stream(9 +5*(i-1)) = int(modulo(i, 6), kind=int8)
        stream(10+5*(i-1)) = int(modulo(i, 14), kind=int8)
    end do
    call unpack_stream(mp_s, stream, mpv, status)
    deallocate(stream)
    if (.not.(status)) then
        print *, "[Error: issue occurred with unpacking stream(map32)"
        stop 1
    end if
    if (mpv%numelements() /= 262144) then
        print *, "[Error: unpacked map32 contains ", mpv%numelements(), &
            " elements instead of 262144"
        stop 1
    end if
    ! check elements
    call get_map_ref(mpv, maptmp, status)
    if (.not.(status)) then
        print *, "[Error: did not unpack mp_map_type"
        stop 1
    end if
    do i = 1,262144
        ! check key
        call get_int(maptmp%keys(i)%obj, itmp, status)
        if (.not.(status)) then
            print *, "[Error: map32 key", i, "is not an int"
            stop 1
        end if
        if (itmp /= 21 + modulo(i, 16)) then
            print *, "[Error: map32 key", i, "is not", 21 + modulo(i, 16)
            stop 1
        end if
        ! check value
        call get_ext_ref(maptmp%values(i)%obj, exttmp, status)
        if (.not.(status)) then
            print *, "[Error: map32 value", i, "is not an ext"
            stop 1
        end if
        if (exttmp%numelements() /= 2 .or. &
                exttmp%values(1) /= modulo(i, 6) .or. &
                exttmp%values(2) /= modulo(i, 14)) then
            print *, "[Error: map32 value", i, "has invalid data"
            stop 1
        end if
    end do
    deallocate(mpv)
    print *, "[Info: map32 test succeeded"

    ! bin8 test
    ! values = x
    i_tmp = 30
    allocate(stream(i_tmp + 2))
    stream(1) = MP_B8
    stream(2) = 30
    do i = 1,i_tmp
        stream(i + 2) = int(i, kind=int8)
    end do
    call unpack_stream(mp_s, stream, mpv, status)
    deallocate(stream)
    if (.not.(status)) then
        print *, "[Error: issue occurred with unpacking stream(bin)"
        stop 1
    end if
    ! check that binary was unpacked
    if (.not. is_bin(mpv)) then
        print *, "[Error: Did not unpack binary"
        stop 1
    end if
    ! check length of the array
    if (mpv%numelements() /= i_tmp) then
        print *, "[Error: unpacked bin8 contains ", mpv%numelements(), " elements instead of", i_tmp
        stop 1
    end if

    call get_bin(mpv, byte_tmp, status)
    deallocate(mpv)
    print *, "[Info: bin8 test succeeded"

    ! bin16 test
    ! values = x % 35
    i_tmp = 60000 ! 0xea60
    allocate(stream(i_tmp + 3))
    stream(1) = MP_B16
    stream(2) = -22 ! 0xea 
    stream(3) =  96 ! 0x60
    do i = 1,i_tmp
        stream(i + 3) = int(modulo(i, 35), kind=int8)
    end do
    call unpack_stream(mp_s, stream, mpv, status)
    deallocate(stream)
    if (.not.(status)) then
        print *, "[Error: issue occurred with unpacking stream(bin)"
        stop 1
    end if
    ! check that binary was unpacked
    if (.not. is_bin(mpv)) then
        print *, "[Error: Did not unpack binary"
        stop 1
    end if
    ! check length of the array
    if (mpv%numelements() /= i_tmp) then
        print *, "[Error: unpacked bin16 contains ", mpv%numelements(), " elements instead of", i_tmp
        stop 1
    end if

    call get_bin(mpv, byte_tmp, status)
    deallocate(mpv)
    print *, "[Info: bin16 test succeeded"

    ! bin32 test
    ! values = (x%25) * (x%14)
    i_tmp = 100000 ! 0x000186a0
    allocate(stream(i_tmp + 5))
    stream(1) = MP_B32
    stream(2) = 0 ! 0x00
    stream(3) = 1 ! 0x01
    stream(4) = -122 ! 0x86
    stream(5) = -96 ! 0xa0
    do i = 1,i_tmp
        stream(i + 5) = int(modulo(i, 25) * modulo(i, 14), kind=int8)
    end do
    call unpack_stream(mp_s, stream, mpv, status)
    deallocate(stream)
    if (.not.(status)) then
        print *, "[Error: issue occurred with unpacking stream(bin)"
        stop 1
    end if
    ! check that binary was unpacked
    if (.not. is_bin(mpv)) then
        print *, "[Error: Did not unpack binary"
        stop 1
    end if
    ! check length of the array
    if (mpv%numelements() /= i_tmp) then
        print *, "[Error: unpacked bin32 contains ", mpv%numelements(), " elements instead of", i_tmp
        stop 1
    end if

    call get_bin(mpv, byte_tmp, status)
    deallocate(mpv)
    print *, "[Info: bin32 test succeeded"

    ! fixext1 test
    ! type = 4
    ! values = [2]
    allocate(stream(3))
    stream(1) = MP_FE1
    stream(2) = 4 ! type
    stream(3) = 2 ! data

    call unpack_stream(mp_s, stream, mpv, status)
    deallocate(stream)
    if (.not.(status)) then
        print *, "[Error: issue occurred with unpacking stream(ext)"
        stop 1
    end if
    ! check that ext was unpacked
    if (.not. is_ext(mpv)) then
        print *, "[Error: Did not unpack extension"
        stop 1
    end if
    ! check length of data
    if (mpv%numelements() /= 1) then
        print *, "[Error: unpacked fixext1 contains ", mpv%numelements(), " elements instead of 1"
        stop 1
    end if
    ! check type and data
    call get_ext_ref(mpv, exttmp, status)
    if (exttmp%exttype /= 4) then
        print *, "[Error: unpacked fixext1 has type ", exttmp%exttype, " instead of expected type 4"
        stop 1
    end if
    if (exttmp%values(1) /= 2) then
        print *, "[Error: unpacked fixext1 has unexpected data"
        stop 1
    end if

    deallocate(mpv)
    print *, "[Info: fixext1 test succeeded"

    ! fixext2 test
    ! type = -5
    ! values = [-1, -2]
    allocate(stream(4))
    stream(1) = MP_FE2
    stream(2) = -5 ! type
    stream(3) = -1 ! data
    stream(4) = -2

    call unpack_stream(mp_s, stream, mpv, status)
    deallocate(stream)
    if (.not.(status)) then
        print *, "[Error: issue occurred with unpacking stream(ext)"
        stop 1
    end if
    ! check that ext was unpacked
    if (.not. is_ext(mpv)) then
        print *, "[Error: Did not unpack extension"
        stop 1
    end if
    ! check length of data
    if (mpv%numelements() /= 2) then
        print *, "[Error: unpacked fixext2 contains ", mpv%numelements(), " elements instead of 1"
        stop 1
    end if
    ! check type and data
    call get_ext_ref(mpv, exttmp, status)
    if (exttmp%exttype /= -5) then
        print *, "[Error: unpacked fixext2 has type ", exttmp%exttype, " instead of expected type -5"
        stop 1
    end if
    if (exttmp%values(1) /= -1 .or. exttmp%values(2) /= -2) then
        print *, "[Error: unpacked fixext2 has unexpected data"
        stop 1
    end if

    deallocate(mpv)
    print *, "[Info: fixext2 test succeeded"

    ! fixext4 test
    ! type = 100
    ! values = [0, 10, 100, -20]
    allocate(stream(6))
    stream(1) = MP_FE4
    stream(2) = 100 ! type
    stream(3) = 0 ! data
    stream(4) = 10
    stream(5) = 100
    stream(6) = -20

    call unpack_stream(mp_s, stream, mpv, status)
    deallocate(stream)
    if (.not.(status)) then
        print *, "[Error: issue occurred with unpacking stream(ext)"
        stop 1
    end if
    ! check that ext was unpacked
    if (.not. is_ext(mpv)) then
        print *, "[Error: Did not unpack extension"
        stop 1
    end if
    ! check length of data
    if (mpv%numelements() /= 4) then
        print *, "[Error: unpacked fixext4 contains ", mpv%numelements(), " elements instead of 4"
        stop 1
    end if
    ! check type and data
    call get_ext_ref(mpv, exttmp, status)
    if (exttmp%exttype /= 100) then
        print *, "[Error: unpacked fixext4 has type ", exttmp%exttype, " instead of expected type 100"
        stop 1
    end if

    deallocate(mpv)
    print *, "[Info: fixext4 test succeeded"

    ! fixext8 test
    ! type = 127
    ! values = [1, 2, 3, 4, 5, 6, 7, 8]
    allocate(stream(10))
    stream(1) = MP_FE8
    stream(2) = 127 ! type
    do i = 1,8
        stream(2+i) = int(i, kind=int8)
    end do

    call unpack_stream(mp_s, stream, mpv, status)
    deallocate(stream)
    if (.not.(status)) then
        print *, "[Error: issue occurred with unpacking stream(ext)"
        stop 1
    end if
    ! check that ext was unpacked
    if (.not. is_ext(mpv)) then
        print *, "[Error: Did not unpack extension"
        stop 1
    end if
    ! check length of data
    if (mpv%numelements() /= 8) then
        print *, "[Error: unpacked fixext8 contains ", mpv%numelements(), " elements instead of 8"
        stop 1
    end if
    ! check type and data
    call get_ext_ref(mpv, exttmp, status)
    if (exttmp%exttype /= 127) then
        print *, "[Error: unpacked fixext8 has type ", exttmp%exttype, " instead of expected type 127"
        stop 1
    end if

    deallocate(mpv)
    print *, "[Info: fixext8 test succeeded"

    ! fixext16 test
    ! type = 127
    ! values = [-1, -2, ...., -16]
    allocate(stream(18))
    stream(1) = MP_FE16
    stream(2) = -128 ! type
    do i = 1,16
        stream(2+i) = int(-i, kind=int8)
    end do

    call unpack_stream(mp_s, stream, mpv, status)
    deallocate(stream)
    if (.not.(status)) then
        print *, "[Error: issue occurred with unpacking stream(ext)"
        stop 1
    end if
    ! check that ext was unpacked
    if (.not. is_ext(mpv)) then
        print *, "[Error: Did not unpack extension"
        stop 1
    end if
    ! check length of data
    if (mpv%numelements() /= 16) then
        print *, "[Error: unpacked fixext16 contains ", mpv%numelements(), " elements instead of 16"
        stop 1
    end if
    ! check type and data
    call get_ext_ref(mpv, exttmp, status)
    if (exttmp%exttype /= -128) then
        print *, "[Error: unpacked fixext16 has type ", exttmp%exttype, " instead of expected type -128"
        stop 1
    end if

    deallocate(mpv)
    print *, "[Info: fixext16 test succeeded"

    ! ext8 test
    ! type = 17
    ! values = [1, 2, 3]
    allocate(stream(6))
    stream(1) = MP_E8
    stream(2) = 3
    stream(3) = 17
    stream(4) = 1
    stream(5) = 2
    stream(6) = 3

    call unpack_stream(mp_s, stream, mpv, status)
    deallocate(stream)
    if (.not.(status)) then
        print *, "[Error: issue occurred with unpacking stream(ext)"
        stop 1
    end if
    ! check that ext was unpacked
    if (.not. is_ext(mpv)) then
        print *, "[Error: Did not unpack extension"
        stop 1
    end if
    ! check length of data
    if (mpv%numelements() /= 3) then
        print *, "[Error: unpacked ext8 contains ", mpv%numelements(), " elements instead of 3"
        stop 1
    end if
    ! check type and data
    call get_ext_ref(mpv, exttmp, status)
    if (exttmp%exttype /= 17) then
        print *, "[Error: unpacked ext8 has type ", exttmp%exttype, " instead of expected type 17"
        stop 1
    end if
    if (.not. all(exttmp%values .eq. (/1,2,3/))) then
        print *, "[Error: unpacked ext8 does not contain expected data"
        stop 1
    end if

    deallocate(mpv)
    print *, "[Info: ext8 test succeeded"

    ! ext16 test
    ! type = 23
    ! values: [400 ... 1] N = 400 (0x0190)
    allocate(stream(404))
    stream(1) = MP_E16
    stream(2) = 1    ! 0x01
    stream(3) = -112 ! 0x90
    stream(4) = 23
    do i = 1,400
        stream(4 + i) = int(401 - i, kind=int8)
    end do

    call unpack_stream(mp_s, stream, mpv, status)
    deallocate(stream)
    if (.not.(status)) then
        print *, "[Error: issue occurred with unpacking stream(ext)"
        stop 1
    end if
    ! check that ext was unpacked
    if (.not. is_ext(mpv)) then
        print *, "[Error: Did not unpack extension"
        stop 1
    end if
    ! check length of data
    if (mpv%numelements() /= 400) then
        print *, "[Error: unpacked ext16 contains ", mpv%numelements(), " elements instead of 400"
        stop 1
    end if
    ! check type and data
    call get_ext_ref(mpv, exttmp, status)
    if (exttmp%exttype /= 23) then
        print *, "[Error: unpacked ext16 has type ", exttmp%exttype, " instead of expected type 23"
        stop 1
    end if
    do i = 1,400
        if (exttmp%values(i) /= int(401 - i, kind=int8)) then
            print *, "[Error: unpacked ext16 does not contain expected data"
            stop 1
        end if
    end do

    deallocate(mpv)
    print *, "[Info: ext16 test succeeded"

    ! ext32 test
    ! type = 100
    ! values: [400 ... 1] N = 80000 (0x00013880)
    allocate(stream(80006))
    stream(1) = MP_E32
    stream(2) = 0    ! 0x00
    stream(3) = 1    ! 0x01
    stream(4) = 56   ! 0x38
    stream(5) = -128 ! 0x80
    stream(6) = 100
    do i = 1,80000
        stream(6 + i) = int(modulo(i, 3_int8), kind=int8)
    end do

    call unpack_stream(mp_s, stream, mpv, status)
    deallocate(stream)
    if (.not.(status)) then
        print *, "[Error: issue occurred with unpacking stream(ext)"
        stop 1
    end if
    ! check that ext was unpacked
    if (.not. is_ext(mpv)) then
        print *, "[Error: Did not unpack extension"
        stop 1
    end if
    ! check length of data
    if (mpv%numelements() /= 80000) then
        print *, "[Error: unpacked ext32 contains ", mpv%numelements(), " elements instead of 80000"
        stop 1
    end if
    ! check type and data
    call get_ext_ref(mpv, exttmp, status)
    if (exttmp%exttype /= 100) then
        print *, "[Error: unpacked ext32 has type ", exttmp%exttype, " instead of expected type 100"
        stop 1
    end if
    do i = 1,80000
        if (exttmp%values(i) /= modulo(i, 3_int8)) then
            print *, "[Error: unpacked ext32 does not contain expected data"
            stop 1
        end if
    end do

    deallocate(mpv)
    print *, "[Info: ext32 test succeeded"

    ! extension testing
    ! timestamp32 test
    ! 300 seconds => 0x0000012c
    allocate(stream(6))
    stream(1) = MP_FE4
    stream(2) = -1_int8
    stream(3) = 0  ! 0x00
    stream(4) = 0  ! 0x00
    stream(5) = 1  ! 0x01
    stream(6) = 44 ! 0x2c

    call unpack_stream(mp_s, stream, mpv, status)
    deallocate(stream)
    if (.not.(status)) then
        print *, "[Error: issue occurred with unpacking (timestamp32)"
        stop 1
    end if
    ! check that timestamp was unpacked
    if (.not. is_timestamp(mpv)) then
        print *, "[Error: Did not unpack timestamp"
        stop 1
    end if
    ! check type and data
    call get_timestamp_ref(mpv, ts_tmp, status)
    if (ts_tmp%seconds /= 300 .or. ts_tmp%nanoseconds /= 0) then
        print *, "[Error: unpacked timestamp32: ", ts_tmp%seconds, ts_tmp%nanoseconds
        print *, "    while expected 300s"
        stop 1
    end if

    deallocate(mpv)
    print *, "[Info: timestamp32 test succeeded"

    ! timestamp64 test
    ! 2^30 seconds, 2^5 ns
    ! 0x00000080 40000000  this is hard to calculate
    allocate(stream(10))
    stream(1) = MP_FE8
    stream(2) = -1_int8
    stream(3) = 0 ! 0x00
    stream(4) = 0 ! 0x00
    stream(5) = 0 ! 0x00
    stream(6) = -128 ! 0x80
    stream(7) = 64 ! 0x40
    stream(8) = 0 ! 0x00
    stream(9) = 0 ! 0x00
    stream(10) = 0 ! 0x00

    call unpack_stream(mp_s, stream, mpv, status)
    deallocate(stream)
    if (.not.(status)) then
        print *, "[Error: issue occurred with unpacking (timestamp64)"
        stop 1
    end if
    ! check that timestamp was unpacked
    if (.not. is_timestamp(mpv)) then
        print *, "[Error: Did not unpack timestamp"
        stop 1
    end if
    ! check type and data
    call get_timestamp_ref(mpv, ts_tmp, status)
    if (ts_tmp%seconds /= 1073741824_int64 .or. &
            ts_tmp%nanoseconds /= 32) then
        print *, "[Error: unpacked timestamp64: ", ts_tmp%seconds, ts_tmp%nanoseconds
        print *, "    while expected 1073741824 seconds, 32 ns"
        stop 1
    end if

    print *, "[Info: timestamp64 test succeeded"

    ! timestamp96 test
    ! -200 seconds, 30 ns
    ! 0x0000001e
    ! 0xffffffffffffff38
    allocate(stream(15))
    stream(1)  = MP_E8
    stream(2)  = 12
    stream(3)  = -1
    stream(4)  = 0  ! 0x00
    stream(5)  = 0  ! 0x00
    stream(6)  = 0  ! 0x00
    stream(7)  = 30 ! 0x1e
    stream(8)  = -1 ! 0xff
    stream(9)  = -1 ! 0xff
    stream(10) = -1 ! 0xff
    stream(11) = -1 ! 0xff
    stream(12) = -1 ! 0xff
    stream(13) = -1 ! 0xff
    stream(14) = -1 ! 0xff
    stream(15) = 56 ! 0x38

    call unpack_stream(mp_s, stream, mpv, status)
    deallocate(stream)
    if (.not.(status)) then
        print *, "[Error: issue occurred with unpacking (timestamp96)"
        stop 1
    end if
    ! check that timestamp was unpacked
    if (.not. is_timestamp(mpv)) then
        print *, "[Error: Did not unpack timestamp"
        stop 1
    end if
    ! check type and data
    call get_timestamp_ref(mpv, ts_tmp, status)
    if (ts_tmp%seconds /= -200 .or. &
            ts_tmp%nanoseconds /= 30) then
        print *, "[Error: unpacked timestamp64: ", ts_tmp%seconds, ts_tmp%nanoseconds
        print *, "    while expected -200 seconds, 30 ns"
        stop 1
    end if

    print *, "[Info: timestamp96 test succeeded"

end program
