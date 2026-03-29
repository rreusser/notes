program test_ztrttp
  use test_utils
  implicit none
  integer, parameter :: NMAX = 4
  complex*16 :: A(NMAX, NMAX), AP(NMAX*(NMAX+1)/2)
  complex*16 :: Apk(NMAX*NMAX)
  double precision :: AP_r(NMAX*(NMAX+1))
  double precision :: Apk_r(NMAX*NMAX*2)
  equivalence (AP, AP_r)
  equivalence (Apk, Apk_r)
  integer :: i, j, info

  ! Test 1: lower triangular, 3x3
  ! Set up A as a full 3x3 complex matrix (column-major in NMAX x NMAX)
  A = (0.0d0, 0.0d0)
  A(1,1) = (1.0d0, 0.1d0)
  A(2,1) = (2.0d0, 0.2d0)
  A(3,1) = (3.0d0, 0.3d0)
  A(1,2) = (99.0d0, 99.0d0)  ! upper part, not referenced
  A(2,2) = (5.0d0, 0.5d0)
  A(3,2) = (6.0d0, 0.6d0)
  A(1,3) = (99.0d0, 99.0d0)
  A(2,3) = (99.0d0, 99.0d0)
  A(3,3) = (9.0d0, 0.9d0)
  AP = (0.0d0, 0.0d0)
  call ztrttp('L', 3, A, NMAX, AP, info)
  call begin_test('lower_3x3')
  call print_int('info', info)
  call print_array('AP', AP_r, 12)
  call end_test()

  ! Test 2: upper triangular, 3x3
  A = (0.0d0, 0.0d0)
  A(1,1) = (1.0d0, -0.1d0)
  A(1,2) = (4.0d0, -0.4d0)
  A(1,3) = (7.0d0, -0.7d0)
  A(2,1) = (99.0d0, 99.0d0)  ! lower part, not referenced
  A(2,2) = (5.0d0, -0.5d0)
  A(2,3) = (8.0d0, -0.8d0)
  A(3,1) = (99.0d0, 99.0d0)
  A(3,2) = (99.0d0, 99.0d0)
  A(3,3) = (9.0d0, -0.9d0)
  AP = (0.0d0, 0.0d0)
  call ztrttp('U', 3, A, NMAX, AP, info)
  call begin_test('upper_3x3')
  call print_int('info', info)
  call print_array('AP', AP_r, 12)
  call end_test()

  ! Test 3: lower triangular, 4x4
  A = (0.0d0, 0.0d0)
  do j = 1, 4
    do i = j, 4
      A(i, j) = dcmplx(dble(i + (j-1)*4), dble(i + (j-1)*4) * 0.01d0)
    end do
  end do
  AP = (0.0d0, 0.0d0)
  call ztrttp('L', 4, A, NMAX, AP, info)
  call begin_test('lower_4x4')
  call print_int('info', info)
  call print_array('AP', AP_r, 20)
  call end_test()

  ! Test 4: upper triangular, 4x4
  A = (0.0d0, 0.0d0)
  do j = 1, 4
    do i = 1, j
      A(i, j) = dcmplx(dble(i + (j-1)*4), dble(-(i + (j-1)*4)) * 0.1d0)
    end do
  end do
  AP = (0.0d0, 0.0d0)
  call ztrttp('U', 4, A, NMAX, AP, info)
  call begin_test('upper_4x4')
  call print_int('info', info)
  call print_array('AP', AP_r, 20)
  call end_test()

  ! Test 5: N=0 (quick return)
  AP = (-1.0d0, -1.0d0)
  call ztrttp('L', 0, A, NMAX, AP, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 6: N=1 lower
  A(1,1) = (42.0d0, -3.5d0)
  AP(1) = (0.0d0, 0.0d0)
  call ztrttp('L', 1, A, NMAX, AP, info)
  call begin_test('n_one_lower')
  call print_int('info', info)
  call print_array('AP', AP_r, 2)
  call end_test()

  ! Test 7: N=1 upper
  A(1,1) = (77.0d0, 1.25d0)
  AP(1) = (0.0d0, 0.0d0)
  call ztrttp('U', 1, A, NMAX, AP, info)
  call begin_test('n_one_upper')
  call print_int('info', info)
  call print_array('AP', AP_r, 2)
  call end_test()

  ! Test 8: lower 3x3 with LDA > N (LDA = NMAX = 4, N = 3)
  ! Same data as test 1
  A = (0.0d0, 0.0d0)
  A(1,1) = (1.0d0, 0.1d0)
  A(2,1) = (2.0d0, 0.2d0)
  A(3,1) = (3.0d0, 0.3d0)
  A(2,2) = (5.0d0, 0.5d0)
  A(3,2) = (6.0d0, 0.6d0)
  A(3,3) = (9.0d0, 0.9d0)
  AP = (0.0d0, 0.0d0)
  call ztrttp('L', 3, A, NMAX, AP, info)
  call begin_test('lower_3x3_lda4')
  call print_int('info', info)
  call print_array('AP', AP_r, 12)
  call end_test()

  ! Test 9: upper 3x3 with LDA > N (LDA = NMAX = 4, N = 3)
  A = (0.0d0, 0.0d0)
  A(1,1) = (1.0d0, -0.1d0)
  A(1,2) = (4.0d0, -0.4d0)
  A(1,3) = (7.0d0, -0.7d0)
  A(2,2) = (5.0d0, -0.5d0)
  A(2,3) = (8.0d0, -0.8d0)
  A(3,3) = (9.0d0, -0.9d0)
  AP = (0.0d0, 0.0d0)
  call ztrttp('U', 3, A, NMAX, AP, info)
  call begin_test('upper_3x3_lda4')
  call print_int('info', info)
  call print_array('AP', AP_r, 12)
  call end_test()

end program
