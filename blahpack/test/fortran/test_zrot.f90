program test_zrot
  use test_utils
  implicit none
  complex*16 :: cx(10), cy(10)
  double precision :: cx_r(20), cy_r(20)
  equivalence (cx, cx_r)
  equivalence (cy, cy_r)
  double precision :: c
  complex*16 :: s
  integer :: n

  ! Test 1: basic rotation with c=cos(pi/4), s=(sin(pi/4),0)
  ! c = 1/sqrt(2), s = (1/sqrt(2), 0)
  n = 3
  c = 1.0d0 / sqrt(2.0d0)
  s = dcmplx(1.0d0 / sqrt(2.0d0), 0.0d0)
  cx(1) = (1.0d0, 0.0d0)
  cx(2) = (0.0d0, 1.0d0)
  cx(3) = (1.0d0, 1.0d0)
  cy(1) = (0.0d0, 0.0d0)
  cy(2) = (1.0d0, 0.0d0)
  cy(3) = (0.0d0, -1.0d0)
  call zrot(n, cx, 1, cy, 1, c, s)
  call begin_test('zrot_basic')
  call print_array('cx', cx_r, 2*n)
  call print_array('cy', cy_r, 2*n)
  call end_test()

  ! Test 2: n=0 (no-op)
  cx(1) = (1.0d0, 2.0d0)
  cy(1) = (3.0d0, 4.0d0)
  call zrot(0, cx, 1, cy, 1, c, s)
  call begin_test('zrot_n_zero')
  call print_array('cx', cx_r, 2)
  call print_array('cy', cy_r, 2)
  call end_test()

  ! Test 3: identity rotation c=1, s=(0,0)
  n = 2
  c = 1.0d0
  s = (0.0d0, 0.0d0)
  cx(1) = (1.0d0, 2.0d0)
  cx(2) = (3.0d0, 4.0d0)
  cy(1) = (5.0d0, 6.0d0)
  cy(2) = (7.0d0, 8.0d0)
  call zrot(n, cx, 1, cy, 1, c, s)
  call begin_test('zrot_identity')
  call print_array('cx', cx_r, 2*n)
  call print_array('cy', cy_r, 2*n)
  call end_test()

  ! Test 4: complex s, c=0.6, s=(0.0,0.8)
  n = 2
  c = 0.6d0
  s = (0.0d0, 0.8d0)
  cx(1) = (1.0d0, 0.0d0)
  cx(2) = (0.0d0, 1.0d0)
  cy(1) = (0.0d0, 1.0d0)
  cy(2) = (1.0d0, 0.0d0)
  call zrot(n, cx, 1, cy, 1, c, s)
  call begin_test('zrot_complex_s')
  call print_array('cx', cx_r, 2*n)
  call print_array('cy', cy_r, 2*n)
  call end_test()

  ! Test 5: non-unit stride incx=2, incy=2
  n = 2
  c = 0.0d0
  s = (1.0d0, 0.0d0)
  cx(1) = (1.0d0, 0.0d0)
  cx(2) = (99.0d0, 99.0d0)
  cx(3) = (2.0d0, 0.0d0)
  cy(1) = (3.0d0, 0.0d0)
  cy(2) = (99.0d0, 99.0d0)
  cy(3) = (4.0d0, 0.0d0)
  call zrot(n, cx, 2, cy, 2, c, s)
  call begin_test('zrot_stride')
  call print_array('cx', cx_r, 6)
  call print_array('cy', cy_r, 6)
  call end_test()

end program
