module messagepack

    ! implement buffer or c++ vector
    ! implement static buffer?
    ! implement packing
    ! implement unpacking
    ! implement file io
    use iso_fortran_env
    use messagepack_value
    use messagepack_pack
    use messagepack_unpack
    use byte_utilities
    implicit none
contains
    subroutine print_version()
        print *, "0.0.1"
    end subroutine

    subroutine print_messagepack(obj)
        class(mp_value_type) :: obj
        call print_messagepack_with_args(obj, 0, .false.)
    end subroutine

    recursive subroutine print_messagepack_with_args(obj, indentation, sameline)
        class(mp_value_type), intent(in) :: obj
        integer, intent(in) :: indentation
        logical, intent(in) :: sameline
        integer(kind=int64) :: i, j

        if (.not. sameline) then
            do i = 1,indentation
                write(*, "(A)", advance="no") char(9)
            end do
        end if

        select type(obj)
        type is (mp_value_type)
        class is (mp_nil_type)
            write(*, "(A)", advance="no") "nil"
        class is (mp_bool_type)
            if (obj%value) then
                write(*, "(A)", advance="no") "true"
            else
                write(*, "(A)", advance="no") "false"
            end if
        class is (mp_int_type)
            if (obj%unsigned_64) then
                write(*, "(I0, A)", advance="no") obj%value, "[OUT-OF-RANGE]"
            else
                write(*, "(I0)", advance="no") obj%value
            end if
            
        class is (mp_float_type)
            if (obj%is_64) then
                write(*, "(F0.0)", advance="no") obj%f64value
            else
                write(*, "(F0.0)", advance="no") obj%f32value
            end if
        class is (mp_str_type)
            write(*, "(A, A, A)", advance="no") char(34), obj%value, char(34)
        class is (mp_bin_type)
            print *, "binary value TODO"
        class is (mp_arr_type)
            write(*, "(A, I0, A)") "array[", obj%numelements(), "]"
            do i = 1,indentation+1
                write(*, "(A)", advance="no") char(9)
            end do
            write(*, "(A)", advance="no") "["
            do j = 1,obj%numelements()
                call print_messagepack_with_args(obj%value(j)%obj, 0, .true.)
                write(*, "(A)", advance="no") ", " 
            end do
            write(*, "(A)", advance="no") "]" 
        class is (mp_map_type)
            write(*, "(A, I0, A)") "map[", obj%numelements(), "]"
            do j = 1, obj%numelements()
                do i = 1,indentation+1
                    write(*, "(A)", advance="no") char(9)
                end do
                call print_messagepack_with_args(obj%keys(j)%obj, indentation + 1, .true.)
                write(*, "(A)", advance="no") " => "
                call print_messagepack_with_args(obj%values(j)%obj, indentation + 1, .true.)
                print *, ""
            end do
        class is (mp_ext_type)
            print *, "ext value TODO"
        end select
        if (.not. sameline) then
            print *, ""
        end if
    end subroutine

end module
