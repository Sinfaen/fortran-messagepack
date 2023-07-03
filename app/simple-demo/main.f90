! Example demonstrating roundtrip serialization
! & deserialization.
program simple_demo
    use messagepack
    implicit none

    ! variables
    class(mp_map_type), allocatable :: mp_map
    class(mp_arr_type), allocatable :: mp_arr
    class(mp_bin_type), allocatable :: mp_bin
    class(msgpack), allocatable :: mp
    byte, dimension(:), allocatable :: buffer


    print *, 'Simple MessagePack Demo'

    mp = msgpack()
    ! treat extra bytes as an error during unpacking
    call mp%extra_bytes_is_error(.true.)

    ! complicated example
    mp_arr = mp_arr_type(3_int64)
    mp_arr%values(1)%obj = mp_float_type(5.01)
    mp_arr%values(2)%obj = mp_float_type(-21.2)
    mp_arr%values(3)%obj = mp_float_type(700.0)

    mp_bin = mp_bin_type(2_int64)
    mp_bin%values(1) = -2
    mp_bin%values(2) = 35

    mp_map = mp_map_type(6_int64) ! pairs
    mp_map%keys(1)%obj   = mp_str_type("rat")
    mp_map%values(1)%obj = mp_int_type(5)
    mp_map%keys(2)%obj   = mp_str_type("gerbil")
    mp_map%values(2)%obj = mp_bin
    mp_map%keys(3)%obj   = mp_str_type("capybara")
    mp_map%values(3)%obj = mp_int_type(11)
    mp_map%keys(4)%obj   = mp_str_type("jerboa")
    mp_map%values(4)%obj = mp_int_type(2)
    mp_map%keys(5)%obj   = mp_bool_type(.true.)
    mp_map%values(5)%obj = mp_arr
    mp_map%keys(6)%obj   = mp_int_type(0)
    mp_map%values(6)%obj = mp_int_type(-11)

    ! print the structure to the user
    print *, "MessagePack map object to be serialized"
    call mp%print_value(mp_map)

    ! pack the data
    call mp%pack_alloc(mp_map, buffer)
    if (mp%failed()) then
        print *, "[Error: failed to pack mp_map"
        print *, mp%error_message
        stop 1
    end if
    ! print the buffer
    print *, "Serialized Data:"
    call print_bytes_as_hex(buffer, .true.)

    deallocate(buffer)
    deallocate(mp_map)
    deallocate(mp_arr)
    deallocate(mp_bin)
end program
