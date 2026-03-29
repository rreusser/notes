program test_ztpttr
  use test_utils
  implicit none
  integer, parameter :: NMAX = 4
  complex*16 :: AP(NMAX*(NMAX+1)/2), A(NMAX, NMAX)
  complex*16 :: Apk(NMAX*NMAX)
  double precision :: AP_r(NMAX*(NMAX+1))
  double precision :: Apk_r(NMAX*NMAX*2)
  equivalence (AP, AP_r)
  equivalence (Apk, Apk_r)
  integer :: i, j, info

  ! Test 1: lower triangular, 4x4
  ! Packed storage for lower: column by column, j=1..N, i=j..N
  ! AP has N*(N+1)/2 = 10 complex elements
  do i = 1, 10
    AP(i) = dcmplx(dble(i), dble(i) * 0.1d0)
  end do
  A = (0.0d0, 0.0d0)
  call ztpttr('L', 4, AP, A, NMAX, info)
  ! Pack A into contiguous array for printing
  do j = 1, 4
    do i = 1, 4
      Apk(i + (j-1)*4) = A(i, j)
    end do
  end do
  call begin_test('lower_4x4')
  call print_int('info', info)
  call print_array('A', Apk_r, 32)
  call print_array('AP', AP_r, 20)
  call end_test()

  ! Test 2: upper triangular, 4x4
  do i = 1, 10
    AP(i) = dcmplx(dble(i) * 10.0d0, dble(i) * (-0.5d0))
  end do
  A = (0.0d0, 0.0d0)
  call ztpttr('U', 4, AP, A, NMAX, info)
  do j = 1, 4
    do i = 1, 4
      Apk(i + (j-1)*4) = A(i, j)
    end do
  end do
  call begin_test('upper_4x4')
  call print_int('info', info)
  call print_array('A', Apk_r, 32)
  call print_array('AP', AP_r, 20)
  call end_test()

  ! Test 3: n=0 (quick return)
  A = (99.0d0, 99.0d0)
  call ztpttr('L', 0, AP, A, NMAX, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 4: n=1 lower
  AP(1) = (42.0d0, -3.5d0)
  A = (0.0d0, 0.0d0)
  call ztpttr('L', 1, AP, A, NMAX, info)
  do j = 1, 1
    do i = 1, 1
      Apk(i + (j-1)*1) = A(i, j)
    end do
  end do
  call begin_test('n_one_lower')
  call print_int('info', info)
  call print_array('A', Apk_r, 2)
  call end_test()

  ! Test 5: n=1 upper
  AP(1) = (77.0d0, 1.25d0)
  A = (0.0d0, 0.0d0)
  call ztpttr('U', 1, AP, A, NMAX, info)
  do j = 1, 1
    do i = 1, 1
      Apk(i + (j-1)*1) = A(i, j)
    end do
  end do
  call begin_test('n_one_upper')
  call print_int('info', info)
  call print_array('A', Apk_r, 2)
  call end_test()

  ! Test 6: lower triangular, 3x3
  AP(1) = (1.5d0, 0.5d0)
  AP(2) = (2.5d0, 1.5d0)
  AP(3) = (3.5d0, 2.5d0)
  AP(4) = (4.5d0, 3.5d0)
  AP(5) = (5.5d0, 4.5d0)
  AP(6) = (6.5d0, 5.5d0)
  A = (0.0d0, 0.0d0)
  call ztpttr('L', 3, AP, A, NMAX, info)
  do j = 1, 3
    do i = 1, 3
      Apk(i + (j-1)*3) = A(i, j)
    end do
  end do
  call begin_test('lower_3x3')
  call print_int('info', info)
  call print_array('A', Apk_r, 18)
  call print_array('AP', AP_r, 12)
  call end_test()

  ! Test 7: upper triangular, 3x3
  AP(1) = (11.0d0, -1.0d0)
  AP(2) = (12.0d0, -2.0d0)
  AP(3) = (13.0d0, -3.0d0)
  AP(4) = (14.0d0, -4.0d0)
  AP(5) = (15.0d0, -5.0d0)
  AP(6) = (16.0d0, -6.0d0)
  A = (0.0d0, 0.0d0)
  call ztpttr('U', 3, AP, A, NMAX, info)
  do j = 1, 3
    do i = 1, 3
      Apk(i + (j-1)*3) = A(i, j)
    end do
  end do
  call begin_test('upper_3x3')
  call print_int('info', info)
  call print_array('A', Apk_r, 18)
  call print_array('AP', AP_r, 12)
  call end_test()

end program
