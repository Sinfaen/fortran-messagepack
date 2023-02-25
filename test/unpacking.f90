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
end program
