program test_dstev
  use test_utils
  implicit none
  double precision :: d(10), e(10), z(10, 10), work(20)
  double precision :: z_pack(100)
  integer :: info, i, j, n_val

  ! Test 1: JOBZ='N' — eigenvalues only, 5x5 tridiagonal
  ! Matrix:
  !   2 -1  0  0  0
  !  -1  2 -1  0  0
  !   0 -1  2 -1  0
  !   0  0 -1  2 -1
  !   0  0  0 -1  2
  n_val = 5
  d(1:5) = (/ 2.0d0, 2.0d0, 2.0d0, 2.0d0, 2.0d0 /)
  e(1:4) = (/ -1.0d0, -1.0d0, -1.0d0, -1.0d0 /)
  call dstev('N', n_val, d, e, z, 10, work, info)
  call begin_test('eigenvalues_only_5x5')
  call print_int('N', n_val)
  call print_array('d', d, n_val)
  call print_int('info', info)
  call end_test()

  ! Test 2: JOBZ='V' — eigenvalues + eigenvectors, 5x5
  d(1:5) = (/ 2.0d0, 2.0d0, 2.0d0, 2.0d0, 2.0d0 /)
  e(1:4) = (/ -1.0d0, -1.0d0, -1.0d0, -1.0d0 /)
  call dstev('V', n_val, d, e, z, 10, work, info)
  call begin_test('eigenvectors_5x5')
  call print_int('N', n_val)
  call print_array('d', d, n_val)
  ! Pack z(1:5, 1:5) into z_pack
  do j = 1, n_val
    do i = 1, n_val
      z_pack((j-1)*n_val + i) = z(i, j)
    end do
  end do
  call print_array('z', z_pack, n_val*n_val)
  call print_int('info', info)
  call end_test()

  ! Test 3: JOBZ='V' — 4x4 non-uniform diagonal
  n_val = 4
  d(1:4) = (/ 4.0d0, 1.0d0, 3.0d0, 2.0d0 /)
  e(1:3) = (/ 1.0d0, 0.5d0, 1.5d0 /)
  call dstev('V', n_val, d, e, z, 10, work, info)
  call begin_test('eigenvectors_4x4')
  call print_int('N', n_val)
  call print_array('d', d, n_val)
  do j = 1, n_val
    do i = 1, n_val
      z_pack((j-1)*n_val + i) = z(i, j)
    end do
  end do
  call print_array('z', z_pack, n_val*n_val)
  call print_int('info', info)
  call end_test()

  ! Test 4: n=0 quick return
  call dstev('N', 0, d, e, z, 1, work, info)
  call begin_test('n_zero')
  call print_int('N', 0)
  call print_int('info', info)
  call end_test()

  ! Test 5: n=1 — trivial
  n_val = 1
  d(1) = 7.5d0
  call dstev('V', n_val, d, e, z, 10, work, info)
  call begin_test('n_one')
  call print_int('N', n_val)
  call print_array('d', d, 1)
  call print_array('z', z(1:1, 1:1), 1)
  call print_int('info', info)
  call end_test()

  ! Test 6: JOBZ='N' — already sorted eigenvalues (diagonal matrix)
  n_val = 4
  d(1:4) = (/ 1.0d0, 2.0d0, 3.0d0, 4.0d0 /)
  e(1:3) = (/ 0.0d0, 0.0d0, 0.0d0 /)
  call dstev('N', n_val, d, e, z, 10, work, info)
  call begin_test('already_sorted')
  call print_int('N', n_val)
  call print_array('d', d, n_val)
  call print_int('info', info)
  call end_test()

end program
