program packing
    use messagepack
    use iso_fortran_env
    implicit none

    ! variables to use
    class(mp_int_type), allocatable :: int_test
    byte, allocatable, dimension(:) :: buf
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
    if (size(buf) /= 1 .and. buf(1) /= 4_int8) then
        print *, "[Error: packing of the value 4 failed"
        stop 1
    end if
    deallocate(buf)
    deallocate(int_test)
    print *, "[Info: PFI packing test succeeded"

    ! negative fix int test
    int_test = mp_int_type(-5)
    call pack_alloc(int_test, buf, errored)
    if (errored) then
        print *, "[Error: failed to pack int"
        stop 1
    end if
    ! we expect a single byte that contains the value -26 (0b11100110 as int8)
    if (size(buf) /= 1 .and. buf(1) /= -26_int8) then
        print *, "[Error: packing of the value -5 failed"
        stop 1
    end if
    deallocate(buf)
    deallocate(int_test)
    print *, "[Info: NFI packing test succeeded"

end program
