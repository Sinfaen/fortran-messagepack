! Example demonstrating the stream-like capabilities of this
! library.
! A buffer with multiple messagepack values stored in it
! is iterated over until there are no more complete values
! left to unpack.
program unpack_buffer
    use messagepack
    implicit none

    ! variables
    class(mp_value_type), allocatable :: mpv
    class(msgpack), allocatable :: mp
    byte, dimension(:), allocatable :: buffer

    integer(kind=int64) :: index, numbytes

    print *, 'Unpack Buffer MessagePack Demo'
    mp = msgpack()

    ! put three MessagePack values side by side
    ! -33
    ! true
    ! ["abba", nil]
    allocate(buffer(10))
    buffer(:) = (/-48,-33,-61,-110,-92,97,98,98,97,-64/)
    index = 1

    ! deserialize and then print each object to the console
    do while (mp%is_available(buffer(index:)))
        call mp%unpack_buf(buffer(index:), mpv, numbytes)
        if (mp%failed()) then
            print *, "[Error: failed to unpack"
            print *, mp%error_message
            stop 1
        end if
        print *, "Unpacked:"
        call mp%print_value(mpv)
        print *, ""

        deallocate(mpv)
        index = index + numbytes
        if (index > size(buffer)) then
            exit
        end if
    end do
    print *, "No more values to unpack"
end program
