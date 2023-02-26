# fortran-messagepack
Prototype library for messagepack support in fortran. A shared library with the basename `messagepack` is compiled. 

Utilizes OOP.

## Requirements
- Fortran 2008
- `meson` build system (only one supported for now)

## Supported DataTypes
- Signed Integers
- Unsigned Integers
    - exception: values between 2^63 & 2^64 - 1

## Examples

### Unpacking
```fortran
program test
    use messagepack
    use iso_fortran_env
    implicit none
    ! buffer filled with MsgPack data
    byte, allocatable, dimension(:) :: buffer

    ! base class used to interact with MsgPack
    class(mp_value_type), allocatable :: mp_deserialized
    logical :: error ! error flag

    integer :: unpacked_value

    call unpack_stream(buffer, mp_deserialized, error)
    if (error) then
        Print *, "Failed to read MsgPack data"
        stop 1
    end if

    if (is_int(mp_deserialized)) then
        call get_int(mp_deserialized, unpacked_value, error)
        write(*,*) "Unpacked: ", unpacked_value
    end if

end program
```

## API

### Nil
The underlying support class is `mp_nil_type`.

### Integer Format Family
The underlying support class is `mp_int_type`.

### Float Format Family
 
## TODO
- lots and lots
