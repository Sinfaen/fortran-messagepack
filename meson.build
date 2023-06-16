
project('fortran-messagepack', ['fortran'],
    version : '0.1.1')

my_lib = shared_library('messagepack',
    'src/messagepack.f90',
    'src/messagepack_value.f90',
    'src/messagepack_user.f90',
    'src/messagepack_pack.f90',
    'src/messagepack_unpack.f90',
    'src/byte_utilities.f90')

my_dep = declare_dependency(link_with : my_lib)

e1 = executable('constructors',
    'test/constructors.f90',
    dependencies : [my_dep])
test('constructors', e1)
e2 = executable('unpacking',
    'test/unpacking.f90',
    dependencies : [my_dep])
test('unpacking', e2)
e3 = executable('packing',
    'test/packing.f90',
    dependencies : [my_dep])
test('packing', e3)
