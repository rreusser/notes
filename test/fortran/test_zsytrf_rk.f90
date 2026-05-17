program test_zsytrf_rk
  use test_utils
  implicit none

  ! Use distinctly sized fixed arrays so LDA == N for each test (avoids the
  ! LDA/EQUIVALENCE stride-mismatch bug). Declared sizes match the test sizes
  ! used in the corresponding call.
  complex*16 :: A4(4,4), A5(5,5), A2(2,2), A1(1,1)
  complex*16 :: A33(33,33), A50(50,50)
  complex*16 :: E4(4), E5(5), E2(2), E1(1)
  complex*16 :: E33(33), E50(50)
  complex*16 :: WORK(2000)
  double precision :: A4_r(32), A5_r(50), A2_r(8), A1_r(2)
  double precision :: A33_r(33*33*2), A50_r(50*50*2)
  double precision :: E4_r(8), E5_r(10), E2_r(4), E1_r(2)
  double precision :: E33_r(66), E50_r(100)
  equivalence (A4, A4_r)
  equivalence (A5, A5_r)
  equivalence (A2, A2_r)
  equivalence (A1, A1_r)
  equivalence (A33, A33_r)
  equivalence (A50, A50_r)
  equivalence (E4, E4_r)
  equivalence (E5, E5_r)
  equivalence (E2, E2_r)
  equivalence (E1, E1_r)
  equivalence (E33, E33_r)
  equivalence (E50, E50_r)
  integer :: ipiv4(4), ipiv5(5), ipiv2(2), ipiv1(1)
  integer :: ipiv33(33), ipiv50(50)
  integer :: info, i, j

  ! Test 1: 4x4 complex symmetric, UPLO='L', diagonally dominant (1x1 pivots)
  A4 = (0.0d0, 0.0d0)
  A4(1,1) = (4.0d0, 0.5d0)
  A4(2,1) = (1.0d0, 2.0d0); A4(2,2) = (5.0d0, -0.5d0)
  A4(3,1) = (3.0d0, -1.0d0); A4(3,2) = (2.0d0, 1.0d0); A4(3,3) = (7.0d0, 1.0d0)
  A4(4,1) = (0.5d0, 0.5d0); A4(4,2) = (1.0d0, -2.0d0); A4(4,3) = (3.0d0, 0.0d0); A4(4,4) = (6.0d0, -1.0d0)
  E4 = (0.0d0, 0.0d0); ipiv4 = 0
  call zsytrf_rk('L', 4, A4, 4, E4, ipiv4, WORK, 2000, info)
  call begin_test('4x4_lower')
  call print_array('a', A4_r, 32)
  call print_array('e', E4_r, 8)
  call print_int_array('ipiv', ipiv4, 4)
  call print_int('info', info)
  call end_test()

  ! Test 2: 4x4 complex symmetric, UPLO='U', same matrix
  A4 = (0.0d0, 0.0d0)
  A4(1,1) = (4.0d0, 0.5d0)
  A4(1,2) = (1.0d0, 2.0d0); A4(2,2) = (5.0d0, -0.5d0)
  A4(1,3) = (3.0d0, -1.0d0); A4(2,3) = (2.0d0, 1.0d0); A4(3,3) = (7.0d0, 1.0d0)
  A4(1,4) = (0.5d0, 0.5d0); A4(2,4) = (1.0d0, -2.0d0); A4(3,4) = (3.0d0, 0.0d0); A4(4,4) = (6.0d0, -1.0d0)
  E4 = (0.0d0, 0.0d0); ipiv4 = 0
  call zsytrf_rk('U', 4, A4, 4, E4, ipiv4, WORK, 2000, info)
  call begin_test('4x4_upper')
  call print_array('a', A4_r, 32)
  call print_array('e', E4_r, 8)
  call print_int_array('ipiv', ipiv4, 4)
  call print_int('info', info)
  call end_test()

  ! Test 3: 4x4 truly indefinite (zero diagonal), UPLO='L' — forces 2x2 pivots
  A4 = (0.0d0, 0.0d0)
  A4(2,1) = (1.0d0, 1.0d0)
  A4(3,1) = (2.0d0, -1.0d0); A4(3,2) = (4.0d0, 2.0d0)
  A4(4,1) = (3.0d0, 0.5d0); A4(4,2) = (5.0d0, -1.0d0); A4(4,3) = (6.0d0, 1.5d0)
  E4 = (0.0d0, 0.0d0); ipiv4 = 0
  call zsytrf_rk('L', 4, A4, 4, E4, ipiv4, WORK, 2000, info)
  call begin_test('4x4_indef_lower')
  call print_array('a', A4_r, 32)
  call print_array('e', E4_r, 8)
  call print_int_array('ipiv', ipiv4, 4)
  call print_int('info', info)
  call end_test()

  ! Test 4: 4x4 indefinite, UPLO='U'
  A4 = (0.0d0, 0.0d0)
  A4(1,2) = (1.0d0, 1.0d0)
  A4(1,3) = (2.0d0, -1.0d0); A4(2,3) = (4.0d0, 2.0d0)
  A4(1,4) = (3.0d0, 0.5d0); A4(2,4) = (5.0d0, -1.0d0); A4(3,4) = (6.0d0, 1.5d0)
  E4 = (0.0d0, 0.0d0); ipiv4 = 0
  call zsytrf_rk('U', 4, A4, 4, E4, ipiv4, WORK, 2000, info)
  call begin_test('4x4_indef_upper')
  call print_array('a', A4_r, 32)
  call print_array('e', E4_r, 8)
  call print_int_array('ipiv', ipiv4, 4)
  call print_int('info', info)
  call end_test()

  ! Test 5: N=0 quick return
  ipiv4 = 0; E4 = (0.0d0, 0.0d0)
  call zsytrf_rk('L', 0, A1, 1, E1, ipiv1, WORK, 2000, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 6: N=1
  A1 = (0.0d0, 0.0d0); E1 = (0.0d0, 0.0d0); ipiv1 = 0
  A1(1,1) = (7.0d0, 2.0d0)
  call zsytrf_rk('L', 1, A1, 1, E1, ipiv1, WORK, 2000, info)
  call begin_test('n_one')
  call print_array('a', A1_r, 2)
  call print_array('e', E1_r, 2)
  call print_int_array('ipiv', ipiv1, 1)
  call print_int('info', info)
  call end_test()

  ! Test 7: 2x2 singular (info > 0)
  A2 = (0.0d0, 0.0d0); E2 = (0.0d0, 0.0d0); ipiv2 = 0
  call zsytrf_rk('L', 2, A2, 2, E2, ipiv2, WORK, 2000, info)
  call begin_test('singular_lower')
  call print_array('a', A2_r, 8)
  call print_array('e', E2_r, 4)
  call print_int_array('ipiv', ipiv2, 2)
  call print_int('info', info)
  call end_test()

  ! Test 8: 5x5 mixed 1x1 + 2x2 pivots, UPLO='L', diagonally dominant
  A5 = (0.0d0, 0.0d0)
  A5(1,1) = (4.0d0, 0.2d0)
  A5(2,1) = (1.0d0, -0.3d0); A5(2,2) = (-3.0d0, 0.5d0)
  A5(3,1) = (-2.0d0, 0.1d0); A5(3,2) = (1.0d0, 0.4d0); A5(3,3) = (5.0d0, -0.2d0)
  A5(4,1) = (0.5d0, 0.0d0); A5(4,2) = (2.0d0, -0.1d0); A5(4,3) = (-1.0d0, 0.3d0); A5(4,4) = (2.0d0, 0.5d0)
  A5(5,1) = (1.5d0, 0.2d0); A5(5,2) = (0.0d0, 0.3d0); A5(5,3) = (0.5d0, 0.1d0); A5(5,4) = (1.0d0, -0.1d0); A5(5,5) = (-4.0d0, 0.0d0)
  E5 = (0.0d0, 0.0d0); ipiv5 = 0
  call zsytrf_rk('L', 5, A5, 5, E5, ipiv5, WORK, 2000, info)
  call begin_test('5x5_lower')
  call print_array('a', A5_r, 50)
  call print_array('e', E5_r, 10)
  call print_int_array('ipiv', ipiv5, 5)
  call print_int('info', info)
  call end_test()

  ! Test 9: 5x5 same matrix, UPLO='U'
  A5 = (0.0d0, 0.0d0)
  A5(1,1) = (4.0d0, 0.2d0)
  A5(1,2) = (1.0d0, -0.3d0); A5(2,2) = (-3.0d0, 0.5d0)
  A5(1,3) = (-2.0d0, 0.1d0); A5(2,3) = (1.0d0, 0.4d0); A5(3,3) = (5.0d0, -0.2d0)
  A5(1,4) = (0.5d0, 0.0d0); A5(2,4) = (2.0d0, -0.1d0); A5(3,4) = (-1.0d0, 0.3d0); A5(4,4) = (2.0d0, 0.5d0)
  A5(1,5) = (1.5d0, 0.2d0); A5(2,5) = (0.0d0, 0.3d0); A5(3,5) = (0.5d0, 0.1d0); A5(4,5) = (1.0d0, -0.1d0); A5(5,5) = (-4.0d0, 0.0d0)
  E5 = (0.0d0, 0.0d0); ipiv5 = 0
  call zsytrf_rk('U', 5, A5, 5, E5, ipiv5, WORK, 2000, info)
  call begin_test('5x5_upper')
  call print_array('a', A5_r, 50)
  call print_array('e', E5_r, 10)
  call print_int_array('ipiv', ipiv5, 5)
  call print_int('info', info)
  call end_test()

  ! Test 10: 50x50 well-conditioned (forces blocked path with NB=32), UPLO='L'
  A50 = (0.0d0, 0.0d0); E50 = (0.0d0, 0.0d0); ipiv50 = 0
  do j = 1, 50
    do i = j, 50
      A50(i,j) = cmplx( sin(dble(i)*0.7d0)*cos(dble(j)*0.3d0) + 0.5d0*sin(dble(i+j)*0.13d0), &
                        0.2d0*cos(dble(i)*0.4d0) - 0.1d0*sin(dble(j)*0.5d0), kind=8 )
    end do
    A50(j,j) = cmplx( 50.0d0 + dble(j)*0.1d0, 0.05d0*dble(j), kind=8 )
  end do
  call zsytrf_rk('L', 50, A50, 50, E50, ipiv50, WORK, 2000, info)
  call begin_test('50x50_lower_blocked')
  call print_array('a', A50_r, 5000)
  call print_array('e', E50_r, 100)
  call print_int_array('ipiv', ipiv50, 50)
  call print_int('info', info)
  call end_test()

  ! Test 11: 50x50 well-conditioned, UPLO='U'
  A50 = (0.0d0, 0.0d0); E50 = (0.0d0, 0.0d0); ipiv50 = 0
  do j = 1, 50
    do i = 1, j
      A50(i,j) = cmplx( sin(dble(i)*0.7d0)*cos(dble(j)*0.3d0) + 0.5d0*sin(dble(i+j)*0.13d0), &
                        0.2d0*cos(dble(i)*0.4d0) - 0.1d0*sin(dble(j)*0.5d0), kind=8 )
    end do
    A50(j,j) = cmplx( 50.0d0 + dble(j)*0.1d0, 0.05d0*dble(j), kind=8 )
  end do
  call zsytrf_rk('U', 50, A50, 50, E50, ipiv50, WORK, 2000, info)
  call begin_test('50x50_upper_blocked')
  call print_array('a', A50_r, 5000)
  call print_array('e', E50_r, 100)
  call print_int_array('ipiv', ipiv50, 50)
  call print_int('info', info)
  call end_test()

  ! Test 12: 33x33 well-conditioned (NB=32 + 1-row tail), UPLO='L'
  A33 = (0.0d0, 0.0d0); E33 = (0.0d0, 0.0d0); ipiv33 = 0
  do j = 1, 33
    do i = j, 33
      A33(i,j) = cmplx( sin(dble(i)*1.1d0)*cos(dble(j)*0.5d0) + 0.3d0*sin(dble(i*j)*0.07d0), &
                        0.15d0*cos(dble(i+j)*0.2d0) - 0.07d0*sin(dble(i)*0.6d0), kind=8 )
    end do
    A33(j,j) = cmplx( 33.0d0 + dble(j)*0.05d0, 0.04d0*dble(j), kind=8 )
  end do
  call zsytrf_rk('L', 33, A33, 33, E33, ipiv33, WORK, 2000, info)
  call begin_test('33x33_lower_blocked')
  call print_array('a', A33_r, 33*33*2)
  call print_array('e', E33_r, 66)
  call print_int_array('ipiv', ipiv33, 33)
  call print_int('info', info)
  call end_test()

  ! Test 13: 33x33 same matrix, UPLO='U'
  A33 = (0.0d0, 0.0d0); E33 = (0.0d0, 0.0d0); ipiv33 = 0
  do j = 1, 33
    do i = 1, j
      A33(i,j) = cmplx( sin(dble(i)*1.1d0)*cos(dble(j)*0.5d0) + 0.3d0*sin(dble(i*j)*0.07d0), &
                        0.15d0*cos(dble(i+j)*0.2d0) - 0.07d0*sin(dble(i)*0.6d0), kind=8 )
    end do
    A33(j,j) = cmplx( 33.0d0 + dble(j)*0.05d0, 0.04d0*dble(j), kind=8 )
  end do
  call zsytrf_rk('U', 33, A33, 33, E33, ipiv33, WORK, 2000, info)
  call begin_test('33x33_upper_blocked')
  call print_array('a', A33_r, 33*33*2)
  call print_array('e', E33_r, 66)
  call print_int_array('ipiv', ipiv33, 33)
  call print_int('info', info)
  call end_test()

end program test_zsytrf_rk
