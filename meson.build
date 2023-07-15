
project('fortran_messagepack', ['fortran'],
    version : '0.3.1')

my_lib = shared_library('messagepack',
    'src/messagepack.f90',
    'src/messagepack_value.f90',
    'src/messagepack_user.f90',
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
e4 = executable('roundtrip',
    'test/roundtrip.f90',
    dependencies : [my_dep])
test('roundtrip', e4)

# demo executables
simple_demo = executable('simple-demo',
    'app/simple-demo/main.f90',
    dependencies : [my_dep])
unpack_demo = executable('unpack-buffer',
    'app/unpack-buffer/main.f90',
    dependencies : [my_dep])
