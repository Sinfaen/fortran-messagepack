program constructors
    use messagepack
    implicit none

    type(mp_nil_type)  :: nil_test
    type(mp_bool_type) :: bool_test
    type(mp_int_type)  :: int_test
    type(mp_float_type):: float_test
    type(mp_str_type)  :: str_test
    type(mp_arr_type)  :: arr_test
    type(mp_map_type)  :: map_test
    type(mp_bin_type)  :: bin_test
    type(mp_ext_type)  :: ext_test

    type(mp_timestamp_type) :: timestamp_test

    print *, 'Constructor Test'

    nil_test  = mp_nil_type()
    bool_test = mp_bool_type(.true.)
    int_test  = mp_int_type(45)
    float_test = new_real32(-23.1)
    str_test = mp_str_type("hello world")

    ! array contructor test
    arr_test = mp_arr_type(5_int64)
    arr_test%value(1)%obj = mp_nil_type()
    arr_test%value(2)%obj = mp_bool_type(.true.)
    arr_test%value(3)%obj = mp_int_type(45)
    arr_test%value(4)%obj = new_real32(-51.4)
    arr_test%value(5)%obj = mp_arr_type(2_int64)

    ! map constructor test
    map_test = mp_map_type(4_int64)
    map_test%keys(1)%obj   = mp_int_type(34)
    map_test%values(1)%obj = mp_str_type("tokay gecko")
    map_test%keys(2)%obj   = mp_int_type(10)
    map_test%values(2)%obj = mp_str_type("reticulated python")
    map_test%keys(3)%obj   = mp_int_type(-2)
    map_test%values(3)%obj = mp_str_type("caiman")
    map_test%keys(4)%obj   = mp_int_type(1000)
    map_test%values(4)%obj = mp_nil_type()

    ! binary constructor test
    bin_test = mp_bin_type(3_int64)
    bin_test%value(1) = 3
    bin_test%value(2) = -2
    bin_test%value(3) = 4

    ! extension constructor test
    ext_test = mp_ext_type(40, 7_int64)
    ext_test%values(1) = 4
    ext_test%values(2) = -100
    ext_test%values(3) = 0
    ext_test%values(4) = 99
    ext_test%values(5) = 17
    ext_test%values(6) = -34
    ext_test%values(7) = -42

    ! timestamp test
    timestamp_test = mp_timestamp_type(14, 100023)
end program
