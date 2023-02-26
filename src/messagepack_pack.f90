module messagepack_pack
    use iso_fortran_env
    use,intrinsic :: ieee_arithmetic
    use byte_utilities
    use messagepack_value

    implicit none

    private

    public :: pack

    contains

        subroutine pack(mpv, buffer, error)
            class(mp_value_type) :: mpv
            logical, intent(out) :: error
            byte, allocatable, dimension(:), intent(out) :: buffer
            integer dblen

            call mpv%getsize(dblen) ! get buffer size required
            allocate(buffer(dblen)) ! allocate buffer

            call mpv%pack(buffer, error)
        end subroutine
end module
