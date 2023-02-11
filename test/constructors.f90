program constructors
    use messagepack
    implicit none

    type(mp_nil_type)  :: nil_test
    type(mp_bool_type) :: bool_test
    type(mp_int_type)  :: int_test
    type(mp_float_type):: float_test

    print *, 'Constructor Test'

    nil_test  = mp_nil_type()
    bool_test = mp_bool_type(.true.)
    int_test  = mp_int_type(45)
    float_test = new_real32(-23.1)

end program
