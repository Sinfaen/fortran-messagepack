module messagepack_pack
    use iso_fortran_env
    use,intrinsic :: ieee_arithmetic
    use messagepack_value

    implicit none

    private

    public :: pack_value

    contains

        subroutine pack_value(mpv, buffer, errorcode)
            class(mp_value_type) :: mpv
            integer, intent(out) :: errorcode
            byte, allocatable, dimension(:), intent(out) :: buffer
            integer dblen
            errorcode = 0 ! success

            call mpv%getsize(dblen) ! get buffer size required
            allocate(buffer(dblen)) ! allocate buffer

            ! TODO pack the buffer
        end subroutine
end module
