! Example demonstrating roundtrip serialization
! & deserialization.
program simple_demo
    use messagepack
    implicit none

    ! variables
    class(mp_map_type), allocatable :: mp_map
    logical :: error
    class(mp_settings), allocatable :: mp_s
    byte, dimension(:), allocatable :: buffer


    print *, 'Simple MessagePack Demo'

    mp_s = mp_settings()

    ! map assigning id's to types of rodents
    mp_map = mp_map_type(4_int64) ! pairs
    mp_map%keys(1)%obj   = mp_str_type("rat")
    mp_map%values(1)%obj = mp_int_type(5)
    mp_map%keys(2)%obj   = mp_str_type("gerbil")
    mp_map%values(2)%obj = mp_int_type(2)
    mp_map%keys(3)%obj   = mp_str_type("capybara")
    mp_map%values(3)%obj = mp_int_type(11)
    mp_map%keys(4)%obj   = mp_str_type("jerboa")
    mp_map%values(4)%obj = mp_int_type(2)

    ! print the structure to the user
    print *, "MessagePack map object to be serialized"
    call print_messagepack(mp_map)

    ! pack the data
    call pack_alloc(mp_map, buffer, error)
    if (error) then
        print *, "[Error: failed to pack mp_map"
        stop 1
    end if
    ! print the buffer
    print *, "Serialized Data:"
    call print_bytes_as_hex(buffer)

    deallocate(buffer)
end program
