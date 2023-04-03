program constructors
    use messagepack
    implicit none

    type(mp_nil_type)  :: nil_test
    type(mp_bool_type) :: bool_test
    type(mp_int_type)  :: int_test
    type(mp_float_type):: float_test
    type(mp_str_type)  :: str_test
    type(mp_arr_type)  :: arr_test

    print *, 'Constructor Test'

    nil_test  = mp_nil_type()
    bool_test = mp_bool_type(.true.)
    int_test  = mp_int_type(45)
    float_test = new_real32(-23.1)
    str_test = mp_str_type("hello world")

    ! array contructor test
    arr_test = mp_arr_type(5)
    arr_test%value(1)%obj = mp_nil_type()
    arr_test%value(2)%obj = mp_bool_type(.true.)
    arr_test%value(3)%obj = mp_int_type(45)
    arr_test%value(4)%obj = new_real32(-51.4)
    arr_test%value(5)%obj = mp_arr_type(2)
end program
