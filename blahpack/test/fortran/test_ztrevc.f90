program test_ztrevc
  use test_utils
  implicit none
  integer, parameter :: NMAX = 4
  complex*16 :: T(NMAX, NMAX), VL(NMAX, NMAX), VR(NMAX, NMAX)
  complex*16 :: WORK(2*NMAX)
  double precision :: RWORK(NMAX)
  logical :: SELECT(NMAX)
  integer :: info, n, m, mm, i, j

  ! Packed arrays for output (no padding)
  complex*16 :: VR_packed(NMAX*NMAX), VL_packed(NMAX*NMAX)
  double precision :: VR_p(2*NMAX*NMAX), VL_p(2*NMAX*NMAX)
  equivalence (VR_packed, VR_p)
  equivalence (VL_packed, VL_p)

  ! ==== Test 1: Right eigenvectors, all, 3x3 upper triangular ====
  n = 3
  mm = n
  T = (0.0d0, 0.0d0)
  T(1,1) = (2.0d0, 1.0d0)
  T(1,2) = (1.0d0, 0.5d0)
  T(1,3) = (0.5d0, -0.5d0)
  T(2,2) = (3.0d0, -1.0d0)
  T(2,3) = (1.0d0, 1.0d0)
  T(3,3) = (4.0d0, 0.5d0)
  VR = (0.0d0, 0.0d0)
  call ztrevc('R', 'A', SELECT, n, T, NMAX, VL, NMAX, VR, NMAX, &
              mm, m, WORK, RWORK, info)
  ! Pack VR: copy n x m from VR(NMAX,NMAX) to VR_packed
  do j = 1, m
    do i = 1, n
      VR_packed((j-1)*n + i) = VR(i, j)
    end do
  end do
  call begin_test('right_all_3x3')
  call print_array('VR', VR_p, 2*n*m)
  call print_int('m', m)
  call print_int('info', info)
  call end_test()

  ! ==== Test 2: Left eigenvectors, all, 3x3 ====
  T = (0.0d0, 0.0d0)
  T(1,1) = (2.0d0, 1.0d0)
  T(1,2) = (1.0d0, 0.5d0)
  T(1,3) = (0.5d0, -0.5d0)
  T(2,2) = (3.0d0, -1.0d0)
  T(2,3) = (1.0d0, 1.0d0)
  T(3,3) = (4.0d0, 0.5d0)
  VL = (0.0d0, 0.0d0)
  mm = n
  call ztrevc('L', 'A', SELECT, n, T, NMAX, VL, NMAX, VR, NMAX, &
              mm, m, WORK, RWORK, info)
  do j = 1, m
    do i = 1, n
      VL_packed((j-1)*n + i) = VL(i, j)
    end do
  end do
  call begin_test('left_all_3x3')
  call print_array('VL', VL_p, 2*n*m)
  call print_int('m', m)
  call print_int('info', info)
  call end_test()

  ! ==== Test 3: Both eigenvectors, all, 3x3 ====
  T = (0.0d0, 0.0d0)
  T(1,1) = (2.0d0, 1.0d0)
  T(1,2) = (1.0d0, 0.5d0)
  T(1,3) = (0.5d0, -0.5d0)
  T(2,2) = (3.0d0, -1.0d0)
  T(2,3) = (1.0d0, 1.0d0)
  T(3,3) = (4.0d0, 0.5d0)
  VL = (0.0d0, 0.0d0)
  VR = (0.0d0, 0.0d0)
  mm = n
  call ztrevc('B', 'A', SELECT, n, T, NMAX, VL, NMAX, VR, NMAX, &
              mm, m, WORK, RWORK, info)
  do j = 1, m
    do i = 1, n
      VR_packed((j-1)*n + i) = VR(i, j)
      VL_packed((j-1)*n + i) = VL(i, j)
    end do
  end do
  call begin_test('both_all_3x3')
  call print_array('VR', VR_p, 2*n*m)
  call print_array('VL', VL_p, 2*n*m)
  call print_int('m', m)
  call print_int('info', info)
  call end_test()

  ! ==== Test 4: Right eigenvectors, selected, 3x3 ====
  T = (0.0d0, 0.0d0)
  T(1,1) = (2.0d0, 1.0d0)
  T(1,2) = (1.0d0, 0.5d0)
  T(1,3) = (0.5d0, -0.5d0)
  T(2,2) = (3.0d0, -1.0d0)
  T(2,3) = (1.0d0, 1.0d0)
  T(3,3) = (4.0d0, 0.5d0)
  VR = (0.0d0, 0.0d0)
  SELECT = .FALSE.
  SELECT(1) = .TRUE.
  SELECT(3) = .TRUE.
  mm = 2
  call ztrevc('R', 'S', SELECT, n, T, NMAX, VL, NMAX, VR, NMAX, &
              mm, m, WORK, RWORK, info)
  do j = 1, m
    do i = 1, n
      VR_packed((j-1)*n + i) = VR(i, j)
    end do
  end do
  call begin_test('right_selected_3x3')
  call print_array('VR', VR_p, 2*n*m)
  call print_int('m', m)
  call print_int('info', info)
  call end_test()

  ! ==== Test 5: Backtransform right eigenvectors, 3x3 ====
  T = (0.0d0, 0.0d0)
  T(1,1) = (2.0d0, 1.0d0)
  T(1,2) = (1.0d0, 0.5d0)
  T(1,3) = (0.5d0, -0.5d0)
  T(2,2) = (3.0d0, -1.0d0)
  T(2,3) = (1.0d0, 1.0d0)
  T(3,3) = (4.0d0, 0.5d0)
  VR = (0.0d0, 0.0d0)
  VR(1,1) = (1.0d0, 0.0d0)
  VR(2,2) = (1.0d0, 0.0d0)
  VR(3,3) = (1.0d0, 0.0d0)
  mm = n
  call ztrevc('R', 'B', SELECT, n, T, NMAX, VL, NMAX, VR, NMAX, &
              mm, m, WORK, RWORK, info)
  do j = 1, n
    do i = 1, n
      VR_packed((j-1)*n + i) = VR(i, j)
    end do
  end do
  call begin_test('right_backtransform_3x3')
  call print_array('VR', VR_p, 2*n*n)
  call print_int('m', m)
  call print_int('info', info)
  call end_test()

  ! ==== Test 6: 1x1 matrix ====
  n = 1
  mm = 1
  T = (0.0d0, 0.0d0)
  T(1,1) = (5.0d0, 2.0d0)
  VR = (0.0d0, 0.0d0)
  VL = (0.0d0, 0.0d0)
  call ztrevc('B', 'A', SELECT, n, T, NMAX, VL, NMAX, VR, NMAX, &
              mm, m, WORK, RWORK, info)
  VR_packed(1) = VR(1,1)
  VL_packed(1) = VL(1,1)
  call begin_test('both_all_1x1')
  call print_array('VR', VR_p, 2)
  call print_array('VL', VL_p, 2)
  call print_int('m', m)
  call print_int('info', info)
  call end_test()

  ! ==== Test 7: 4x4 upper triangular ====
  n = 4
  mm = 4
  T = (0.0d0, 0.0d0)
  T(1,1) = (1.0d0, 0.5d0)
  T(1,2) = (2.0d0, -1.0d0)
  T(1,3) = (0.5d0, 0.5d0)
  T(1,4) = (1.0d0, 0.0d0)
  T(2,2) = (3.0d0, 1.0d0)
  T(2,3) = (1.0d0, -0.5d0)
  T(2,4) = (0.5d0, 1.0d0)
  T(3,3) = (2.0d0, -1.0d0)
  T(3,4) = (1.5d0, 0.0d0)
  T(4,4) = (4.0d0, 0.0d0)
  VR = (0.0d0, 0.0d0)
  VL = (0.0d0, 0.0d0)
  call ztrevc('B', 'A', SELECT, n, T, NMAX, VL, NMAX, VR, NMAX, &
              mm, m, WORK, RWORK, info)
  ! n=NMAX=4, so no packing needed
  do j = 1, m
    do i = 1, n
      VR_packed((j-1)*n + i) = VR(i, j)
      VL_packed((j-1)*n + i) = VL(i, j)
    end do
  end do
  call begin_test('both_all_4x4')
  call print_array('VR', VR_p, 2*n*n)
  call print_array('VL', VL_p, 2*n*n)
  call print_int('m', m)
  call print_int('info', info)
  call end_test()

  ! ==== Test 8: Left eigenvectors, selected, 4x4 ====
  T = (0.0d0, 0.0d0)
  T(1,1) = (1.0d0, 0.5d0)
  T(1,2) = (2.0d0, -1.0d0)
  T(1,3) = (0.5d0, 0.5d0)
  T(1,4) = (1.0d0, 0.0d0)
  T(2,2) = (3.0d0, 1.0d0)
  T(2,3) = (1.0d0, -0.5d0)
  T(2,4) = (0.5d0, 1.0d0)
  T(3,3) = (2.0d0, -1.0d0)
  T(3,4) = (1.5d0, 0.0d0)
  T(4,4) = (4.0d0, 0.0d0)
  VL = (0.0d0, 0.0d0)
  SELECT = .FALSE.
  SELECT(2) = .TRUE.
  SELECT(4) = .TRUE.
  mm = 2
  call ztrevc('L', 'S', SELECT, n, T, NMAX, VL, NMAX, VR, NMAX, &
              mm, m, WORK, RWORK, info)
  do j = 1, m
    do i = 1, n
      VL_packed((j-1)*n + i) = VL(i, j)
    end do
  end do
  call begin_test('left_selected_4x4')
  call print_array('VL', VL_p, 2*n*m)
  call print_int('m', m)
  call print_int('info', info)
  call end_test()

end program test_ztrevc
