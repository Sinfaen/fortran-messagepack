program unpacking
    use messagepack
    implicit none

    ! variables to use
    byte, allocatable, dimension(:) :: stream ! buffer of bytes
    class(mp_value_type), allocatable :: mpv  ! pointer to value
    integer :: i
    logical :: btmp
    integer(kind=int64) :: itmp
    real(kind=real64) :: rtmp
    integer(kind=int64), dimension(3) :: i_a_3
    integer(kind=int64), dimension(2) :: i_a_2
    character(:), allocatable :: stmp
    class(mp_arr_type), allocatable :: arrtmp
    class(mp_map_type), allocatable :: maptmp
    logical :: status

    print *, "Unpacking test"
    call print_endianness()

    ! positive fix int test: VALUE = 45
    allocate(stream(1))
    stream(1) = 45
    call unpack_stream(stream, mpv, status)
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
    call unpack_stream(stream, mpv, status)
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
    call unpack_stream(stream, mpv, status)
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
    call unpack_stream(stream, mpv, status)
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
    call unpack_stream(stream, mpv, status)
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
    call unpack_stream(stream, mpv, status)
    deallocate(stream)
    if (.not.(status)) then
        print *, "[Error: issue occurred with unpacking stream(fixarr)"
        stop 1
    end if
    ! check length of the array
    i_a_3 = (/12, -3, 32000/)
    if (mpv%numelements() /= 3) then
        print *, "[Error: unpacked fixarray contains ", mpv%numelements(), " elements instead of 3"
        stop 1
    end if
    select type (mpv)
    type is (mp_value_type)
    class is (mp_arr_type)
        ! loop over all elements
        do i = 1,3
            if (.not. is_int(mpv%value(i)%obj)) then
                print *, "[Error: fixarray[", i, "] is not an int"
                stop 1
            end if
            call get_int(mpv%value(i)%obj, itmp, status)
            if (itmp /= i_a_3(i)) then
                print *, "[Error: unpacked ", itmp, "instead of", i_a_3(i), "for fixarray > ", i
            end if
        end do
    class default
        print *, "[Error: did not unpack mp_arr_type"
        stop 1
    end select
    deallocate(mpv)
    print *, "[Info: Fixarray test succeeded"

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
    call unpack_stream(stream, mpv, status)
    deallocate(stream)
    if (.not.(status)) then
        print *, "[Error: issue occurred with unpacking stream(fixmap)"
        stop 1
    end if
    if (mpv%numelements() /= 3) then
        print *, "[Error: unpacked fixmap contains ", mpv%numelements(), " elements instead of 3"
        stop 1
    end if

    !call print_messagepack(mpv)
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

end program
