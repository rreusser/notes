program test_zlag2c
  use test_utils
  implicit none

  complex*16 :: A(4,4)
  complex    :: SA(4,4)
  complex    :: SApk(4*4)
  real       :: SApk_r(2*4*4)
  double precision :: SApk_d(2*4*4)
  equivalence (SApk, SApk_r)
  integer :: INFO, i, j, m, n, k

  ! ---- Test 1: basic 3x3 conversion ----
  m = 3; n = 3
  do j = 1, n
    do i = 1, m
      A(i,j) = cmplx( dble(i) + 0.1d0*dble(j), -dble(i) + 0.5d0*dble(j), kind=8 )
    end do
  end do
  call ZLAG2C(m, n, A, 4, SA, 4, INFO)
  do j = 1, n
    do i = 1, m
      SApk(i + (j-1)*m) = SA(i,j)
    end do
  end do
  do k = 1, 2*m*n
    SApk_d(k) = dble(SApk_r(k))
  end do
  call begin_test('basic_3x3')
  call print_int('info', INFO)
  call print_array('sa', SApk_d, 2*m*n)
  call end_test()

  ! ---- Test 2: rectangular 2x4 ----
  m = 2; n = 4
  do j = 1, n
    do i = 1, m
      A(i,j) = cmplx( 0.5d0*dble(i*j), 0.25d0*dble(i+j), kind=8 )
    end do
  end do
  call ZLAG2C(m, n, A, 4, SA, 4, INFO)
  do j = 1, n
    do i = 1, m
      SApk(i + (j-1)*m) = SA(i,j)
    end do
  end do
  do k = 1, 2*m*n
    SApk_d(k) = dble(SApk_r(k))
  end do
  call begin_test('rect_2x4')
  call print_int('info', INFO)
  call print_array('sa', SApk_d, 2*m*n)
  call end_test()

  ! ---- Test 3: rectangular 4x2 ----
  m = 4; n = 2
  do j = 1, n
    do i = 1, m
      A(i,j) = cmplx( dble(i)*0.7d0, -dble(j)*1.3d0, kind=8 )
    end do
  end do
  call ZLAG2C(m, n, A, 4, SA, 4, INFO)
  do j = 1, n
    do i = 1, m
      SApk(i + (j-1)*m) = SA(i,j)
    end do
  end do
  do k = 1, 2*m*n
    SApk_d(k) = dble(SApk_r(k))
  end do
  call begin_test('rect_4x2')
  call print_int('info', INFO)
  call print_array('sa', SApk_d, 2*m*n)
  call end_test()

  ! ---- Test 4: m=0 quick exit ----
  m = 0; n = 3
  INFO = -99
  call ZLAG2C(m, n, A, 4, SA, 4, INFO)
  call begin_test('m_zero')
  call print_int('info', INFO)
  call end_test()

  ! ---- Test 5: n=0 quick exit ----
  m = 3; n = 0
  INFO = -99
  call ZLAG2C(m, n, A, 4, SA, 4, INFO)
  call begin_test('n_zero')
  call print_int('info', INFO)
  call end_test()

  ! ---- Test 6: 1x1 ----
  m = 1; n = 1
  A(1,1) = cmplx( 3.14159265358979d0, -2.71828182845904d0, kind=8 )
  call ZLAG2C(m, n, A, 4, SA, 4, INFO)
  SApk(1) = SA(1,1)
  do k = 1, 2
    SApk_d(k) = dble(SApk_r(k))
  end do
  call begin_test('one_by_one')
  call print_int('info', INFO)
  call print_array('sa', SApk_d, 2)
  call end_test()

  ! ---- Test 7: overflow — real part too large ----
  m = 2; n = 2
  A(1,1) = cmplx( 1.0d0, 2.0d0, kind=8 )
  A(2,1) = cmplx( 3.0d0, 4.0d0, kind=8 )
  A(1,2) = cmplx( 1.0d300, 0.0d0, kind=8 )
  A(2,2) = cmplx( 5.0d0, 6.0d0, kind=8 )
  call ZLAG2C(m, n, A, 4, SA, 4, INFO)
  call begin_test('overflow_real')
  call print_int('info', INFO)
  call end_test()

  ! ---- Test 8: overflow — imag part too large ----
  m = 2; n = 2
  A(1,1) = cmplx( 1.0d0, 2.0d0, kind=8 )
  A(2,1) = cmplx( 3.0d0, 4.0d0, kind=8 )
  A(1,2) = cmplx( 1.0d0, 1.0d300, kind=8 )
  A(2,2) = cmplx( 5.0d0, 6.0d0, kind=8 )
  call ZLAG2C(m, n, A, 4, SA, 4, INFO)
  call begin_test('overflow_imag')
  call print_int('info', INFO)
  call end_test()

  ! ---- Test 9: overflow — negative real ----
  m = 2; n = 2
  A(1,1) = cmplx( -1.0d300, 0.0d0, kind=8 )
  A(2,1) = cmplx( 3.0d0, 4.0d0, kind=8 )
  A(1,2) = cmplx( 1.0d0, 0.0d0, kind=8 )
  A(2,2) = cmplx( 5.0d0, 6.0d0, kind=8 )
  call ZLAG2C(m, n, A, 4, SA, 4, INFO)
  call begin_test('overflow_neg_real')
  call print_int('info', INFO)
  call end_test()

  ! ---- Test 10: overflow — negative imag ----
  m = 2; n = 2
  A(1,1) = cmplx( 1.0d0, -1.0d300, kind=8 )
  A(2,1) = cmplx( 3.0d0, 4.0d0, kind=8 )
  A(1,2) = cmplx( 1.0d0, 0.0d0, kind=8 )
  A(2,2) = cmplx( 5.0d0, 6.0d0, kind=8 )
  call ZLAG2C(m, n, A, 4, SA, 4, INFO)
  call begin_test('overflow_neg_imag')
  call print_int('info', INFO)
  call end_test()

  ! ---- Test 11: values with small magnitude (no overflow) ----
  m = 3; n = 2
  do j = 1, n
    do i = 1, m
      A(i,j) = cmplx( 1.0d-30*dble(i), 1.0d-30*dble(j), kind=8 )
    end do
  end do
  call ZLAG2C(m, n, A, 4, SA, 4, INFO)
  do j = 1, n
    do i = 1, m
      SApk(i + (j-1)*m) = SA(i,j)
    end do
  end do
  do k = 1, 2*m*n
    SApk_d(k) = dble(SApk_r(k))
  end do
  call begin_test('tiny_values')
  call print_int('info', INFO)
  call print_array('sa', SApk_d, 2*m*n)
  call end_test()

end program
