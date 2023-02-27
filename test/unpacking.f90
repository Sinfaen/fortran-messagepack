program unpacking
    use messagepack
    implicit none

    ! variables to use
    byte, allocatable, dimension(:) :: stream ! buffer of bytes
    class(mp_value_type), allocatable :: mpv  ! pointer to value
    integer(kind=int64) :: itmp
    logical :: stat

    print *, "Unpacking test"
    call print_endianness()

    ! positive fix int test: VALUE = 45
    allocate(stream(1))
    stream(1) = 45
    call unpack_stream(stream, mpv, stat)
    deallocate(stream)
    if (.not.(stat)) then
        print *, "[Error: issue occurred with unpacking stream"
        stop 1
    end if
    if (.not.(is_int(mpv))) then
        print *, "[Error: Did not unpack an int"
        stop 1
    end if
    call get_int(mpv, itmp, stat)
    if (itmp /= 45) then
        write(*,*) "[Error: Unpacked ", itmp, " instead of 45"
        stop 1
    end if
    deallocate(mpv)
    print *, "[Info: PFI test succeeded"

    ! negative fix int test: VALUE = -2
    allocate(stream(1))
    stream(1) = -30 ! 0b11100010 as int8
    call unpack_stream(stream, mpv, stat)
    deallocate(stream)
    if (.not.(stat)) then
        print *, "[Error: issue occurred with unpacking stream"
        stop 1
    end if
    if (.not.(is_int(mpv))) then
        print *, "[Error: Did not unpack an int"
        stop 1
    end if
    call get_int(mpv, itmp, stat)
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
    call unpack_stream(stream, mpv, stat)
    deallocate(stream)
    if (.not.(stat)) then
        print *, "[Error: issue occurred with unpacking stream"
        stop 1
    end if
    if (.not.(is_int(mpv))) then
        print *, "[Error: Did not unpack an int"
        stop 1
    end if
    call get_int(mpv, itmp, stat)
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
    call unpack_stream(stream, mpv, stat)
    deallocate(stream)
    if (.not.(stat)) then
        print *, "[Error: issue occurred with unpacking stream"
        stop 1
    end if
    if (.not.(is_int(mpv))) then
        print *, "[Error: Did not unpack an int"
        stop 1
    end if
    call get_int(mpv, itmp, stat)
    if (itmp /= -9223372036854774463_int64) then
        write(*,*) "[Error: Unpacked (", itmp, ") instead of reinterpreted (-9223372036854774463)"
        stop 1
    end if
    if (.not. is_unsigned(mpv)) then
        print *, "[Error: int was not marked as unsigned"
    end if
    deallocate(mpv)
    print *, "[Info: U64 test succeeded"

end program
