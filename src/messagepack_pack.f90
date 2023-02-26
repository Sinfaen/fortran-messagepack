module messagepack_pack
    use iso_fortran_env
    use,intrinsic :: ieee_arithmetic
    use messagepack_value

    implicit none

    private

    public :: pack_alloc, pack_prealloc

    contains

        subroutine pack_alloc(mpv, buffer, error)
            ! Packs a messagepack object into a dynamically allocated buffer,
            ! returned to the user. The user must handle deallocation of this array
            class(mp_value_type) :: mpv
            logical, intent(out) :: error
            byte, allocatable, dimension(:), intent(out) :: buffer
            integer dblen

            call mpv%getsize(dblen) ! get buffer size required
            allocate(buffer(dblen)) ! allocate buffer

            call mpv%pack(buffer, error)
        end subroutine

        subroutine pack_prealloc(mpv, buffer, error)
            ! Packs a messagepack object into a pre-allocated buffer,
            ! returned to the user. This function does not check beforehand
            ! for the array being the correct size, and will return an error
            ! if the buffer is too small.
            class(mp_value_type) :: mpv
            logical, intent(out) :: error
            byte, allocatable, dimension(:), intent(inout) :: buffer

            call mpv%pack(buffer, error)
        end subroutine
end module
