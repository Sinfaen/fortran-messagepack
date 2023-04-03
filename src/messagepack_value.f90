module messagepack_value
    use iso_fortran_env
    use,intrinsic :: ieee_arithmetic
    use byte_utilities

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
    public :: get_int, set_unsigned, is_unsigned
    public :: get_str

    type :: mp_value_type
        ! nothing here
    contains
        procedure :: getsize => get_size_1
        procedure :: pack => pack_value
        procedure :: numelements => ne_1
    end type

    ! pointer handler for container types
    type :: mp_value_type_ptr
        class(mp_value_type), allocatable :: obj
    end type

    type, extends(mp_value_type) :: mp_nil_type
        ! nothing here
    end type

    type, extends(mp_value_type) :: mp_bool_type
        ! nothing here
        logical :: value
    contains
        procedure :: getsize => get_size_bool
        procedure :: pack => pack_bool
    end type
    interface mp_bool_type
        procedure :: new_bool
    end interface mp_bool_type

    type, extends(mp_value_type) :: mp_int_type
        ! fortran integers are signed. since MsgPack defines unsigned integers,
        ! this needs to handle the case where a uint64 is unpacked, or the user
        ! wants to serialize a uint64, which is the only case where this matters
        ! the `unsigned` flag will go high when this is detected during unpacking
        integer(kind=int64) :: value
        logical :: unsigned_64 = .false.
    contains
        procedure :: getsize => get_size_int
        procedure :: pack => pack_int
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
        procedure :: pack => pack_float
    end type

    type, extends(mp_value_type) :: mp_str_type
        character(:), allocatable :: value
    contains
        procedure :: getsize => get_size_str
        procedure :: pack => pack_str
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
        procedure :: numelements => get_arr_size
        procedure :: pack => pack_arr
    end type
    interface mp_arr_type
        procedure :: new_arr
    end interface mp_arr_type

    type, extends(mp_value_type) :: mp_map_type
        class(mp_value_type_ptr), allocatable, dimension(:) :: keys
        class(mp_value_type_ptr), allocatable, dimension(:) :: values
        integer :: ne
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
            integer(kind=int64), intent(out) :: osize
            osize = 1
        end subroutine

        integer function ne_1(obj)
            class(mp_value_type) :: obj
            ne_1 = 1
        end function

        subroutine get_size_bool(this, osize)
            class(mp_bool_type) :: this
            integer(kind=int64), intent(out) :: osize
            osize = 1
        end subroutine

        subroutine get_size_int(this, osize)
            class(mp_int_type) :: this
            integer(kind=int64), intent(out) :: osize
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
                    osize = 9 ! int64 & uint64
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
                    osize = 9 ! uint64 & int64
                end if
            end if
        end subroutine

        subroutine get_size_float(this, osize)
            class(mp_float_type) :: this
            integer(kind=int64), intent(out) :: osize
            if (this%is_64) then
                osize = 9 ! real64
            else
                osize = 5 ! real32
            end if
        end subroutine

        integer function get_str_type(length)
            ! get type of string based on length of the string
            integer(kind=int64), intent(in) :: length
            if (length <= 31) then
                get_str_type = MP_FS_L + int(length, kind=int8)
            else if (length <= 255) then
                get_str_type = MP_S8
            else if (length <= 65535) then
                get_str_type = MP_S16
            else if (length <= 4294967295_int64) then
                get_str_type = MP_S32
            else
                get_str_type = MP_NU ! bad
            end if
        end function

        integer function get_arr_type(length)
            ! get type of array based on length of the array
            integer(kind=int64), intent(in) :: length
            if (length <= 15) then
                get_arr_type = int(ior(MP_FA_L, int(length)), kind=int8)
            else if (length <= 65535) then
                get_arr_type = MP_A16
            else if (length <= 4294967295_int64) then
                get_arr_type = MP_A32
            else
                get_arr_type = MP_NU ! bad
            end if
        end function

        subroutine get_size_str(this, osize)
            class(mp_str_type)   :: this
            integer(kind=int64), intent(out) :: osize
            integer(kind=int64) :: length
            length = len(this%value)
            select case(get_str_type(length))
            case (MP_FS_L:MP_FS_H)
                osize = length + 1
            case (MP_S8)
                osize = length + 2 ! str8
            case (MP_S16)
                osize = length + 3 ! str16
            case (MP_S32)
                osize = length + 5 ! str32
            case default
                osize = 0
                print *, "WARNING BAD STRING"
            end select
        end subroutine

        subroutine get_size_bin(this, osize)
            class(mp_bin_type)   :: this
            integer(kind=int64), intent(out) :: osize
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
            integer(kind=int64), intent(out) :: osize
            integer(kind=int64) i, elemsize, length

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
            integer(kind=int64), intent(out) :: osize

            integer(kind=int64) keysize, valuesize, i
            ! set initialsize
            if (this%ne <= 15) then
                osize = 1 ! fixmap
            else if (this%ne <= 65535) then
                osize = 3 ! map16
            else
                osize = 5 ! map32
            end if
            ! TODO handle errors for larger

            ! get sizes of all contained values
            do i = 1, this%ne
                call this%keys(i)%obj%getsize(keysize)
                call this%values(i)%obj%getsize(valuesize)
                osize = osize + keysize + valuesize
            end do
        end subroutine

        subroutine get_size_ext(this, osize)
            class(mp_ext_type)   :: this
            integer(kind=int64), intent(out) :: osize
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

        subroutine pack_value(this, buf, error)
            class(mp_value_type) :: this
            byte, dimension(:) :: buf
            logical, intent(out) :: error
            print *, "[Error: abstract pack function called"
            error = .true. ! this function should never be called
        end subroutine

        subroutine pack_bool(this, buf, error)
            class(mp_bool_type) :: this
            byte, dimension(:) :: buf
            logical, intent(out) :: error

            if (size(buf) < 1) then
                error = .true.
                return
            end if

            if (this%value) then
                buf(1) = MP_T
            else
                buf(1) = MP_F
            end if
            error = .false.
        end subroutine

        subroutine pack_int(this, buf, error)
            class(mp_int_type) :: this
            byte, dimension(:) :: buf
            logical, intent(out) :: error

            integer(kind=int64) :: replength
            call this%getsize(replength)
            if (replength > size(buf)) then
                error = .true.
                return
            end if
            error = .false.
            if (this%value < 0) then
                if (this%value >= -32) then
                    ! negative fixint
                    buf(1) = int(-32 - this%value, kind=int8)
                else if (this%value >= -128) then
                    ! int8
                    buf(1) = MP_I8
                    buf(2) = int(this%value, kind=int8)
                else if (this%value >= -32768) then
                    ! int16
                    buf(1) = MP_I16
                    call int_to_bytes_be_2(buf(2:3), int(this%value, kind=int16))
                else if (this%value >= -2147483648_int64) then
                    ! int32
                    buf(1) = MP_I32
                    call int_to_bytes_be_4(buf(2:5), int(this%value, kind=int32))
                else
                    if (this%unsigned_64) then
                        ! uint64
                        buf(1) = MP_U64
                    else
                        ! int64
                        buf(1) = MP_I64
                    end if
                    call int_to_bytes_be_8(buf(2:9), int(this%value, kind=int64))
                end if
            else
                if (this%value <= 127) then
                    buf(1) = int(this%value, kind=int8)
                else if (this%value <= 255) then
                    ! uint8
                    buf(1) = MP_U8
                    buf(2) = int(this%value, kind=int8)
                else if (this%value <= 65535) then
                    ! uint16
                    buf(1) = MP_U16
                    call int_to_bytes_be_2(buf(2:3), int(this%value, kind=int16))
                else if (this%value <= 4294967295_int64) then
                    ! uint32
                    buf(1) = MP_U32
                    call int_to_bytes_be_4(buf(2:5), int(this%value, kind=int32))
                else
                    if (this%unsigned_64) then
                        ! uint64
                        buf(1) = MP_U64
                    else
                        ! int64
                        buf(1) = MP_I64
                    end if
                    call int_to_bytes_be_8(buf(2:9), int(this%value, kind=int64))
                end if
            end if
        end subroutine

        subroutine pack_float(this, buf, error)
            class(mp_float_type) :: this
            byte, dimension(:) :: buf
            logical, intent(out) :: error

            ! check that the buffer can hold the required number of bytes
            integer(kind=int64) :: length
            call this%getsize(length)
            if (length > size(buf)) then
                error = .true.
                return
            end if

            ! serialize value
            if (this%is_64) then
                buf(1) = MP_F64
                call real_to_bytes_be_8(buf(2:9), this%f64value)
            else
                buf(1) = MP_F32
                call real_to_bytes_be_4(buf(2:5), this%f32value)
            end if

            error = .false.
        end subroutine

        subroutine pack_str(this, buf, error)
            class(mp_str_type) :: this
            byte, dimension(:) :: buf
            logical, intent(out) :: error

            ! check that the buffer can hold the required number of bytes
            integer(kind=int64) :: length
            integer :: strtype
            integer :: writeindex
            integer(kind=int64) :: i
            call this%getsize(length)
            if (length > size(buf)) then
                error = .true.
                return
            end if

            ! serialize values
            length = len(this%value)
            strtype = get_str_type(length)
            buf(1) = int(strtype, kind=int8) ! write marker

            select case(strtype)
            case (MP_FS_L:MP_FS_H)
                writeindex = 1
            case (MP_S8)
                writeindex = 2
                buf(2) = int(length, kind=int8)
            case (MP_S16)
                writeindex = 3
                call int_to_bytes_be_2(buf(2:3), int(length, kind=int16))
            case (MP_S32)
                writeindex = 5
                call int_to_bytes_be_4(buf(2:5), int(length, kind=int32))
            end select
            do i = 1,length
                buf(writeindex+i) = transfer(this%value(i:i), 0_int8)
            end do
            error = .false.
        end subroutine

        recursive subroutine pack_arr(this, buf, error)
            class(mp_arr_type) :: this
            byte, dimension(:) :: buf
            logical, intent(out) :: error

            ! check that the buffer can hold the required number of bytes
            integer(kind=int64) :: length
            integer :: arrtype
            integer :: writeindex
            integer(kind=int64) :: i
            call this%getsize(length)
            if (length > size(buf)) then
                error = .true.
                return
            end if

            ! serialize values
            length = this%numelements()
            arrtype = get_arr_type(length)
            buf(1) = int(arrtype, kind=int8) ! write marker

            select case(arrtype)
            case (MP_FA_L:MP_FA_H)
                writeindex = 1
            case (MP_A16)
                writeindex = 3
                call int_to_bytes_be_2(buf(2:3), int(length, kind=int16))
            case (MP_A32)
                writeindex = 5
                call int_to_bytes_be_4(buf(2:5), int(length, kind=int32))
            end select
            do i = 1,length
                call this%value(i)%obj%pack(buf(writeindex +i:), error)
                if (error) then
                    return
                end if
            end do

            error = .false.
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
            ! generic constructor for integers
            integer(kind=int64), intent(in) :: arg
            new_int%value  = arg
        end function new_int

        subroutine set_unsigned(obj)
            ! Changes the unsigned_64 flag to true for packing purposes
            class(mp_value_type), intent(inout) :: obj
            select type (obj)
            type is (mp_value_type)
            class is (mp_int_type)
                obj%unsigned_64 = .true.
            end select
        end subroutine

        logical function is_unsigned(obj)
            class(mp_value_type), intent(in) :: obj
            select type (obj)
            type is (mp_value_type)
            class is (mp_int_type)
                is_unsigned = obj%unsigned_64
            class default
                is_unsigned = .false.
            end select
        end function

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
            character(:), allocatable :: arg
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

        type(mp_arr_type) function new_arr(length)
            integer, intent(in) :: length ! number of elements to allocate
            if (length > 2147483647_int64) then
                print *, "[Warning: Allocated array with size greater than packing allows"
            end if
            allocate(new_arr%value(length))
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

        subroutine get_str(obj, val, stat)
            class(mp_value_type), intent(in) :: obj
            character(:), allocatable, intent(out) :: val
            logical, intent(out) :: stat

            select type (obj)
            type is (mp_value_type)
            class is (mp_str_type)
                val = obj%value
                stat = .true.
            class default
                val = ""
                stat = .false.
            end select
        end subroutine

        integer function get_arr_size(obj)
            class(mp_arr_type) :: obj
            get_arr_size = size(obj%value)
        end function
end module
