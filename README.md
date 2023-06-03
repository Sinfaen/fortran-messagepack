# fortran-messagepack
Prototype library for messagepack support in fortran. A shared library with the basename `messagepack` is compiled. 

Utilizes OOP.

## Requirements
- Fortran 2008
- `meson` build system (only one supported for now)

## Supported Formats
- Nil Format Family
- Int Format Family
- Float Format Family
- Bool Format Family
- String Format Family
- Array Format Family
- Map Format Family
- Bin Format Family
- Extension Format Family

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
All MsgPack classes extend from `mp_value_type`. Container types utilize `mp_value_type_ptr` as a wrapper around the polymorphic type.

```fortran
type :: mp_value_type_ptr
    class(mp_value_type), allocatable :: obj
end type
```

The `mp_value_type` contains the following type-bound procedures:
| Procedure | Applies To | Functionality |
| -- | -- | -- |
| numelements | `mp_arr_type`, `mp_map_type`, `mp_ext_type` | Returns number of contained elements. For the map it returns the number of pairs. For non-containers, returns 1 |
| getsize | All | Returns number of bytes taken up by the object |
| pack | All | Internal use only |

### Nil
The underlying support class is `mp_nil_type`.

```fortran
function is_nil(obj) result(res)
! @returns whether the object is `mp_nil_type`
```

### Integer Format Family
The underlying support class is `mp_int_type`.

```fortran
function is_int(obj) result(res)
! @returns whether the object is a `mp_int_type`

subroutine get_int(obj, val, stat)
! @param[out] val - integer to store decoded value
! @param[out] stat - Returns false if the object is not `mp_int_type`
```

### Float Format Family
The underlying support class is `mp_float_type`.

```fortran
function is_float(obj) result(res)
! @returns whether the object is a `mp_float_type`

subroutine get_real(obj, val, stat)
! @param[out] val - real64 to store decoded value
! @param[out] stat - Returns false if the object is not `mp_float_type`
```

### String Format Family
The underlying support class is `mp_str_type`.

```fortran
function is_str(obj) result(res)
! @returns whether the object is a `mp_str_type`

subroutine get_str(obj, val, stat)
! @param[out] val - character(:), allocatable
! @param[out] stat - Returns false if the object is not `mp_str_type`
```

### Bin Format Family
The underlying support class is `mp_bin_type`.

The constructor of the same name accepts a length argument.

```fortran
type, extends(mp_value_type) :: mp_bin_type
     byte, allocatable, dimension(:) :: value
    ...
contains
    ...
end type
```

Related Functions
```fortran
function is_bin(obj) result(res)
! @returns whether the object is a `mp_bin_type`

subroutine get_bin(obj, val, stat)
! @param[out] val - byte, allocatable, dimension(:)
! @param[out] stat - Returns false if the object is not `mp_bin_type`
```

### Array Format Family
The underlying support class is `mp_arr_type`.

The constructor of the same name accepts a length argument.

```fortran
type, extends(mp_value_type) :: mp_arr_type
    class(mp_value_type_ptr), allocatable, dimension(:) :: value
    ...
contains
    ...
end type
```

Related Functions
```fortran
function is_arr(obj) result(res)
! @returns whether the object is a `mp_arr_type`

subroutine get_arr_ref(obj, val, stat)
! Turn a generic `mp_value_type` into a `mp_arr_type`
! @param[in] obj - class(mp_value_type), allocatable 
! @param[out] val - class(mp_arr_type), allocatable
! @param[out] stat - Returns false if the object is not `mp_arr_type`
```

Example of accessing contained values
```fortran
mp_arr_type :: arr_obj
integer, dimension(3) :: decoded
logical :: status
! arr_obj is pre-populated with 3 ints
do i = 1,3
    get_int(arr_obj%value(i)%obj, decoded(i), status)
    if (.not. status) then
        ! error handling
    end if
end do
print *, "Decoded:", decoded
```

#### Packing Considerations
The length restriction of `(2^32)-1` is only checked for at pack time.

### Map Format Family
The underlying support class is `mp_map_type`.

The constructor of the same name accepts a length argument.

```fortran
type, extends(mp_value_type) :: mp_map_type
    class(mp_value_type_ptr), allocatable, dimension(:) :: keys
    class(mp_value_type_ptr), allocatable, dimension(:) :: values
    ...
contains
    ...
end type
```

Related Functions
```fortran
function is_map(obj) result(res)
! @returns whether the object is a `mp_map_type`

subroutine get_map_ref(obj, val, stat)
! Turn a generic `mp_value_type` into a `mp_map_type`
! @param[in] obj - class(mp_value_type), allocatable 
! @param[out] val - class(mp_map_type), allocatable
! @param[out] stat - Returns false if the object is not `mp_map_type`
```

### Extension Format Family
The underlying support class is `mp_ext_type`.

The constructor of the same name accepts an extension type argument and length argument.

```fortran
type, extends(mp_value_type) :: mp_ext_type
    integer :: exttype ! extension type
    byte, allocatable, dimension(:) :: values
contains
    ...
end type
```

Related Functions
```fortran
function is_ext(obj) result(res)
! @returns whether the object is a `mp_ext_type`

subroutine get_ext_ref(obj, val, stat)
! Turn a generic `mp_value_type` into a `mp_ext_type`
! @param[in] obj - class(mp_value_type), allocatable 
! @param[out] val - class(mp_ext_type), allocatable
! @param[out] stat - Returns false if the object is not `mp_ext_type`
```

## TODO
- customizable extensions
   - timestamp data type

## Tests
Tests integrated into Meson:
| Executable Name | Purpose |
| -- | -- |
| constructors | Unit test of constructors. Also serves as example |
| packing | Unit testing of packing with checks on the packed buffers |
| unpacking | Unit test of unpacking with known packed buffers |

Execute `meson test` to run all tests, or `./<executable_name>` for each test in turn.
