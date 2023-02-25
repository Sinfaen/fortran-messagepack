module messagepack_value
    use iso_fortran_env
    use,intrinsic :: ieee_arithmetic

    implicit none

    ! taken directly from https://github.com/msgpack/msgpack/blob/master/spec.md#formats
    integer, parameter, public :: MP_PFI_L = 0   ! pos fixint low  - 0x00
    integer, parameter, public :: MP_PFI_H = 127 ! pos fixint high - 0x7f
    ! because fortran integers are always signed, we are going to perceive values
    ! as signed even though they're supposed to be unsigned.
    ! the following values are negative as that is how fortran will see them
    integer, parameter, public :: MP_FM_L  = -128 ! fixmap low      - 0x80
    integer, parameter, public :: MP_FM_H  = -113 ! fixmap high     - 0x8f
    integer, parameter, public :: MP_FA_L  = -112 ! fixarray low    - 0x90
    integer, parameter, public :: MP_FA_H  = -97  ! fixarray high   - 0x9f
    integer, parameter, public :: MP_FS_L  = -96  ! fixstr low      - 0xa0
    integer, parameter, public :: MP_FS_H  = -65  ! fixstr high     - 0xbf
    integer, parameter, public :: MP_NIL   = -64  ! nil        - 0xc0
    integer, parameter, public :: MP_NU    = -63  ! never used - 0cx1
    integer, parameter, public :: MP_F     = -62  ! false    - 0xc2
    integer, parameter, public :: MP_T     = -61  ! true     - 0xc3
    integer, parameter, public :: MP_B8    = -60  ! bin8     - 0xc4
    integer, parameter, public :: MP_B16   = -59  ! bin16    - 0xc5
    integer, parameter, public :: MP_B32   = -58  ! bin32    - 0xc6
    integer, parameter, public :: MP_E8    = -57  ! ext8     - 0xc7
    integer, parameter, public :: MP_E16   = -56  ! ext16    - 0xc8
    integer, parameter, public :: MP_E32   = -55  ! ext32    - 0xc9
    integer, parameter, public :: MP_F32   = -54  ! float32  - 0xca
    integer, parameter, public :: MP_F64   = -53  ! float64  - 0xcb
    integer, parameter, public :: MP_U8    = -52  ! uint8    - 0xcc
    integer, parameter, public :: MP_U16   = -51  ! uint16   - 0xcd
    integer, parameter, public :: MP_U32   = -50  ! uint32   - 0xce
    integer, parameter, public :: MP_U64   = -49  ! uint64   - 0xcf
    integer, parameter, public :: MP_I8    = -48  ! int8     - 0xd0
    integer, parameter, public :: MP_I16   = -47  ! int16    - 0xd1
    integer, parameter, public :: MP_I32   = -46  ! int32    - 0xd2
    integer, parameter, public :: MP_I64   = -45  ! int64    - 0xd3
    integer, parameter, public :: MP_FE1   = -44  ! fixext1  - 0xd4
    integer, parameter, public :: MP_FE2   = -43  ! fixext2  - 0xd5
    integer, parameter, public :: MP_FE4   = -42  ! fixext4  - 0xd6
    integer, parameter, public :: MP_FE8   = -41  ! fixext8  - 0xd7
    integer, parameter, public :: MP_FE16  = -40  ! fixext16 - 0xd8
    integer, parameter, public :: MP_S8    = -39  ! str8     - 0xd9
    integer, parameter, public :: MP_S16   = -38  ! str16    - 0xda
    integer, parameter, public :: MP_S32   = -37  ! str32    - 0xdb
    integer, parameter, public :: MP_A16   = -36  ! array16  - 0xdc
    integer, parameter, public :: MP_A32   = -35  ! array32  - 0xdd
    integer, parameter, public :: MP_M16   = -34  ! map16    - 0xde
    integer, parameter, public :: MP_M32   = -33  ! map32    - 0xdf
    integer, parameter, public :: MP_NFI_L = -32  ! neg fixint low  - 0xe0
    integer, parameter, public :: MP_NFI_H = -1   ! neg fixint high - 0xff

    private

    public :: mp_value_type, mp_nil_type, mp_bool_type, mp_int_type, mp_float_type, mp_str_type, mp_bin_type
    public :: mp_arr_type, mp_map_type, mp_ext_type
    public :: is_nil, is_bool, is_int, is_float, is_str, is_bin, is_arr, is_map, is_ext
    public :: new_real32, new_real64
    public :: get_int

    type :: mp_value_type
        ! nothing here
    contains
        procedure :: getsize => get_size_1
        procedure :: pack => pack_value
    end type

    ! pointer handler for container types
    type :: mp_value_type_ptr
        class(mp_value_type), pointer :: obj
    end type

    type, extends(mp_value_type) :: mp_nil_type
        ! nothing here
    end type

    type, extends(mp_value_type) :: mp_bool_type
        ! nothing here
        logical :: value
    end type
    interface mp_bool_type
        procedure :: new_bool
    end interface mp_bool_type

    type, extends(mp_value_type) :: mp_int_type
        ! fortran integers are signed
        ! since MessagePack supports unsigned integers,
        ! this code needs to handle the case where a uint64 is unpacked
        ! the `signed` value will go low when this is detected during unpacking
        integer(kind=int64) :: value
        logical :: signed = .false.
    contains
        procedure :: getsize => get_size_int
    end type
    interface mp_int_type
        procedure :: new_int
    end interface mp_int_type

    type, extends(mp_value_type) :: mp_float_type
        ! simply create memory for both 32bit & 64bit floats
        ! with a logical indicating which one is being used
        real(kind=real64) :: f64value
        real(kind=real32) :: f32value
        logical :: is_64 = .true.
    contains
        procedure :: getsize => get_size_float
    end type

    type, extends(mp_value_type) :: mp_str_type
        character, allocatable :: value
    contains
        procedure :: getsize => get_size_str
    end type
    interface mp_str_type
        procedure :: new_str
    end interface mp_str_type

    type, extends(mp_value_type) :: mp_bin_type
        byte, allocatable, dimension(:) :: value
    contains
        procedure :: getsize => get_size_bin
    end type
    interface mp_bin_type
        procedure :: new_bin
    end interface mp_bin_type

    type, extends(mp_value_type) :: mp_arr_type
        class(mp_value_type_ptr), allocatable, dimension(:) :: value
    contains
        procedure :: getsize => get_size_arr
    end type
    interface mp_arr_type
        procedure :: new_arr
    end interface mp_arr_type

    type, extends(mp_value_type) :: mp_map_type
        class(mp_value_type_ptr), allocatable, dimension(:) :: keys
        class(mp_value_type_ptr), allocatable, dimension(:) :: values
        integer :: numelements
    contains
        procedure :: getsize => get_size_map
    end type
    interface mp_map_type
        procedure :: new_map
    end interface mp_map_type

    type, extends(mp_value_type) :: mp_ext_type
        integer :: exttype
        byte, allocatable, dimension(:) :: value
    contains
        procedure :: getsize => get_size_ext
    end type
    interface mp_ext_type
        procedure :: new_ext
    end interface mp_ext_type

    contains
        subroutine get_size_1(this, osize)
            class(mp_value_type) :: this
            integer, intent(out) :: osize
            osize = 1
        end subroutine

        subroutine get_size_int(this, osize)
            class(mp_int_type) :: this
            integer, intent(out) :: osize
            if (this%value < 0) then
                if (this%value >= -32) then
                    osize = 1 ! negative fixint
                else if (this%value >= -128) then
                    osize = 2 ! int8
                else if (this%value >= -32768) then
                    osize = 3 ! int16
                else if (this%value >= -2147483648_int64) then
                    osize = 5 ! int32
                else
                    osize = 9 ! int64
                end if
            else
                if (this%value <= 127) then
                    osize = 1 ! positive fixint
                else if (this%value <= 255) then
                    osize = 2 ! uint8
                else if (this%value <= 65535) then
                    osize = 3 ! uint16
                else if (this%value <= 4294967295_int64) then
                    osize = 5 ! uint32
                else
                    osize = 5 ! TODO handle uint64
                end if
            end if
        end subroutine

        subroutine get_size_float(this, osize)
            class(mp_float_type) :: this
            integer, intent(out) :: osize
            if (this%is_64) then
                osize = 9 ! real64
            else
                osize = 5 ! real32
            end if
        end subroutine

        subroutine get_size_str(this, osize)
            class(mp_str_type)   :: this
            integer, intent(out) :: osize
            integer :: length
            length = len(this%value)
            if (length <= 31) then
                osize = length + 1 ! fixstr
            else if (length <= 255) then
                osize = length + 2 ! str8
            else if (length <= 65535) then
                osize = length + 3 ! str16
            else
                osize = length + 5 ! str32
            end if
            ! TODO handle longer than error case
        end subroutine

        subroutine get_size_bin(this, osize)
            class(mp_bin_type)   :: this
            integer, intent(out) :: osize
            integer :: length
            length = size(this%value)
            if (length <= 255) then
                osize = length + 2 ! bin8
            else if (length <= 65535) then
                osize = length + 3 ! bin16
            else
                osize = length + 5 ! bin32
            end if
            ! TODO handle longer than error case
        end subroutine

        subroutine get_size_arr(this, osize)
            class(mp_arr_type)   :: this
            integer, intent(out) :: osize
            integer i, elemsize, length

            length = size(this%value)
            ! set initial value
            if (length <= 15) then
                osize = 1 ! fixarray
            else if (length <= 65535) then
                osize = 3 ! array16
            else
                osize = 5 ! array32
            end if
            ! TODO error handling for larger

            ! get sizes of all contained values
            do i = 1, length
                call this%value(i)%obj%getsize(elemsize)
                osize = osize + elemsize
            end do
        end subroutine

        subroutine get_size_map(this, osize)
            class(mp_map_type)   :: this
            integer, intent(out) :: osize

            integer keysize, valuesize, i
            ! set initialsize
            if (this%numelements <= 15) then
                osize = 1 ! fixmap
            else if (this%numelements <= 65535) then
                osize = 3 ! map16
            else
                osize = 5 ! map32
            end if
            ! TODO handle errors for larger

            ! get sizes of all contained values
            do i = 1, this%numelements
                call this%keys(i)%obj%getsize(keysize)
                call this%values(i)%obj%getsize(valuesize)
                osize = osize + keysize + valuesize
            end do
        end subroutine

        subroutine get_size_ext(this, osize)
            class(mp_ext_type)   :: this
            integer, intent(out) :: osize
            integer :: length

            length = size(this%value)
            if (length == 1) then
                osize = 3 ! fixext1
            else if (length == 2) then
                osize = 4 ! fixext2
            else if (length == 4) then
                osize = 6 ! fixext4
            else if (length == 8) then
                osize = 10 ! fixext8
            else if (length == 16) then
                osize = 18 ! fixext16
            else if (length <= 255) then
                osize = 3 + length ! ext8
            else if (length <= 65535) then
                osize = 4 + length ! ext16
            else
                osize = 6 + length ! ext32
            end if
            ! TODO error handling
        end subroutine

        subroutine pack_value(this, ptr)
            class(mp_value_type) :: this
            byte, pointer :: ptr
        end subroutine

        function is_nil(obj) result(res)
            class(mp_value_type), intent(in) :: obj
            logical :: res

            select type (obj)
            type is (mp_value_type)
            class is (mp_nil_type)
                res = .true.
            class default
                res = .false.
            end select
        end function is_nil

        function is_bool(obj) result(res)
            class(mp_value_type), intent(in) :: obj
            logical :: res

            select type (obj)
            type is (mp_value_type)
            class is (mp_bool_type)
                res = .true.
            class default
                res = .false.
            end select
        end function is_bool

        function is_int(obj) result(res)
            class(mp_value_type), intent(in) :: obj
            logical :: res

            select type (obj)
            type is (mp_value_type)
            class is (mp_int_type)
                res = .true.
            class default
                res = .false.
            end select
        end function is_int

        function is_float(obj) result(res)
            class(mp_value_type), intent(in) :: obj
            logical :: res

            select type (obj)
            type is (mp_value_type)
            class is (mp_float_type)
                res = .true.
            class default
                res = .false.
            end select
        end function is_float

        function is_str(obj) result(res)
            class(mp_value_type), intent(in) :: obj
            logical :: res

            select type (obj)
            type is (mp_value_type)
            class is (mp_str_type)
                res = .true.
            class default
                res = .false.
            end select
        end function is_str

        function is_bin(obj) result(res)
            class(mp_value_type), intent(in) :: obj
            logical :: res

            select type (obj)
            type is (mp_value_type)
            class is (mp_bin_type)
                res = .true.
            class default
                res = .false.
            end select
        end function is_bin

        function is_arr(obj) result(res)
            class(mp_value_type), intent(in) :: obj
            logical :: res

            select type (obj)
            type is (mp_value_type)
            class is (mp_arr_type)
                res = .true.
            class default
                res = .false.
            end select
        end function is_arr

        function is_map(obj) result(res)
            class(mp_value_type), intent(in) :: obj
            logical :: res

            select type (obj)
            type is (mp_value_type)
            class is (mp_map_type)
                res = .true.
            class default
                res = .false.
            end select
        end function is_map

        function is_ext(obj) result(res)
            class(mp_value_type), intent(in) :: obj
            logical :: res

            select type (obj)
            type is (mp_value_type)
            class is (mp_ext_type)
                res = .true.
            class default
                res = .false.
            end select
        end function is_ext

        type(mp_bool_type) function new_bool(arg)
            logical, intent(in) :: arg
            new_bool%value = arg
        end function new_bool

        type(mp_int_type) function new_int(arg)
            integer(kind=int64), intent(in) :: arg
            new_int%value  = arg
        end function new_int

        type(mp_float_type) function new_real32(arg)
            real(kind=real32), intent(in) :: arg
            new_real32%f32value = arg
            new_real32%f64value = 0.0
            new_real32%is_64 = .false.
        end function new_real32

        type(mp_float_type) function new_real64(arg)
            real(kind=real64), intent(in) :: arg
            new_real64%f32value = 0.0
            new_real64%f64value = arg
            new_real64%is_64 = .true.
        end function new_real64

        type(mp_str_type) function new_str(arg)
            character, allocatable :: arg
            new_str%value = arg
        end function new_str

        type(mp_bin_type) function new_bin(arg, stat)
            byte, allocatable, dimension(:) :: arg
            logical, intent(out) :: stat
            integer :: l
            new_bin%value = arg
            l = size(new_bin%value)
            if (l > 2147483647_int64) then
                stat = .false. ! too long
            else
                stat = .true.
            end if
        end function new_bin

        type(mp_arr_type) function new_arr(arg, stat)
            class(mp_value_type_ptr), allocatable, dimension(:) :: arg
            logical, intent(out) :: stat
            integer :: l
            new_arr%value = arg
            l = size(new_arr%value)
            if (l > 2147483647_int64) then
                stat = .false. ! too long
            else
                stat = .true.
            end if
        end function new_arr

        type(mp_map_type) function new_map(keys, values, stat)
            class(mp_value_type_ptr), allocatable, dimension(:) :: keys
            class(mp_value_type_ptr), allocatable, dimension(:) :: values
            logical, intent(out) :: stat
            integer :: l

            l = size(keys)
            stat = .true.
            if (l /= size(values)) then
                stat = .false.
            end if
            if (l > 2147483647_int64) then
                stat = .false.
            end if

            new_map%keys   = keys
            new_map%values = values
        end function new_map

        type(mp_ext_type) function new_ext(exttype, data, stat)
            integer :: exttype
            byte, allocatable, dimension(:) :: data
            logical, intent(out) :: stat
            integer :: l

            new_ext%exttype = exttype
            new_ext%value   = data

            l = size(new_ext%value)
            stat = (l <= 2147483647_int64)
        end function new_ext

        subroutine get_int(obj, val, stat)
            class(mp_value_type), intent(in) :: obj
            integer(kind=int64), intent(out) :: val
            logical, intent(out) :: stat
            ! emulate is_int
            select type (obj)
            type is (mp_value_type)
            class is (mp_int_type)
                val = obj%value
                stat = .true.
            class default
                val  = 0
                stat = .false.
            end select
        end subroutine
end module
