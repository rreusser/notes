program test_dsytri2
  use test_utils
  implicit none

  integer, parameter :: NMAX = 40
  double precision :: A(NMAX, NMAX)
  double precision :: WORK_TF(NMAX*64)
  double precision :: WORK_2((NMAX+34)*36)
  integer :: IPIV(NMAX), INFO, n, LWORK_TF, LWORK_2, i, j

  LWORK_TF = NMAX * 64
  LWORK_2 = (NMAX + 34) * 36

  ! Test 1: 4x4 upper symmetric (1x1 pivots)
  n = 4
  A = 0.0d0
  A(1,1) = 4.0d0
  A(1,2) = 1.0d0;   A(2,2) = 5.0d0
  A(1,3) = 3.0d0;   A(2,3) = 2.0d0;   A(3,3) = 6.0d0
  A(1,4) = 0.5d0;   A(2,4) = 1.0d0;   A(3,4) = 3.0d0;   A(4,4) = 8.0d0
  call DSYTRF('U', n, A, NMAX, IPIV, WORK_TF, LWORK_TF, INFO)
  call begin_test('4x4_upper_def')
  call print_int('info_trf', INFO)
  call print_array('a_factored', A, NMAX*n)
  call print_int_array('ipiv', IPIV, n)
  call DSYTRI2('U', n, A, NMAX, IPIV, WORK_2, LWORK_2, INFO)
  call print_int('info', INFO)
  call print_array('a_inv', A, NMAX*n)
  call end_test()

  ! Test 2: 4x4 lower symmetric (1x1 pivots)
  n = 4
  A = 0.0d0
  A(1,1) = 4.0d0
  A(2,1) = 1.0d0;   A(2,2) = 5.0d0
  A(3,1) = 3.0d0;   A(3,2) = 2.0d0;   A(3,3) = 6.0d0
  A(4,1) = 0.5d0;   A(4,2) = 1.0d0;   A(4,3) = 3.0d0;   A(4,4) = 8.0d0
  call DSYTRF('L', n, A, NMAX, IPIV, WORK_TF, LWORK_TF, INFO)
  call begin_test('4x4_lower_def')
  call print_int('info_trf', INFO)
  call print_array('a_factored', A, NMAX*n)
  call print_int_array('ipiv', IPIV, n)
  call DSYTRI2('L', n, A, NMAX, IPIV, WORK_2, LWORK_2, INFO)
  call print_int('info', INFO)
  call print_array('a_inv', A, NMAX*n)
  call end_test()

  ! Test 3: 4x4 upper indefinite (forces 2x2 pivots)
  n = 4
  A = 0.0d0
  A(1,1) = 0.0d0
  A(1,2) = 1.0d0;   A(2,2) = 0.0d0
  A(1,3) = 2.0d0;   A(2,3) = 4.0d0;   A(3,3) = 0.0d0
  A(1,4) = 3.0d0;   A(2,4) = 5.0d0;   A(3,4) = 6.0d0;   A(4,4) = 0.0d0
  call DSYTRF('U', n, A, NMAX, IPIV, WORK_TF, LWORK_TF, INFO)
  call begin_test('4x4_upper_indef')
  call print_int('info_trf', INFO)
  call print_array('a_factored', A, NMAX*n)
  call print_int_array('ipiv', IPIV, n)
  call DSYTRI2('U', n, A, NMAX, IPIV, WORK_2, LWORK_2, INFO)
  call print_int('info', INFO)
  call print_array('a_inv', A, NMAX*n)
  call end_test()

  ! Test 4: 4x4 lower indefinite
  n = 4
  A = 0.0d0
  A(1,1) = 0.0d0
  A(2,1) = 1.0d0;  A(2,2) = 0.0d0
  A(3,1) = 2.0d0;  A(3,2) = 4.0d0;  A(3,3) = 0.0d0
  A(4,1) = 3.0d0;  A(4,2) = 5.0d0;  A(4,3) = 6.0d0;  A(4,4) = 0.0d0
  call DSYTRF('L', n, A, NMAX, IPIV, WORK_TF, LWORK_TF, INFO)
  call begin_test('4x4_lower_indef')
  call print_int('info_trf', INFO)
  call print_array('a_factored', A, NMAX*n)
  call print_int_array('ipiv', IPIV, n)
  call DSYTRI2('L', n, A, NMAX, IPIV, WORK_2, LWORK_2, INFO)
  call print_int('info', INFO)
  call print_array('a_inv', A, NMAX*n)
  call end_test()

  ! Test 5: 5x5 lower mixed pivots
  n = 5
  A = 0.0d0
  A(1,1) = 1.0d0
  A(2,1) = 2.0d0; A(2,2) = 0.0d0
  A(3,1) = 0.0d0; A(3,2) = 3.0d0; A(3,3) = 4.0d0
  A(4,1) = 1.0d0; A(4,2) = 0.0d0; A(4,3) = 2.0d0; A(4,4) = 0.0d0
  A(5,1) = 1.0d0; A(5,2) = 2.0d0; A(5,3) = 0.0d0; A(5,4) = 1.0d0; A(5,5) = 5.0d0
  call DSYTRF('L', n, A, NMAX, IPIV, WORK_TF, LWORK_TF, INFO)
  call begin_test('5x5_lower_mixed')
  call print_int('info_trf', INFO)
  call print_array('a_factored', A, NMAX*n)
  call print_int_array('ipiv', IPIV, n)
  call DSYTRI2('L', n, A, NMAX, IPIV, WORK_2, LWORK_2, INFO)
  call print_int('info', INFO)
  call print_array('a_inv', A, NMAX*n)
  call end_test()

  ! Test 6: 5x5 upper mixed pivots
  n = 5
  A = 0.0d0
  A(1,1) = 1.0d0
  A(1,2) = 2.0d0; A(2,2) = 0.0d0
  A(1,3) = 0.0d0; A(2,3) = 3.0d0; A(3,3) = 4.0d0
  A(1,4) = 1.0d0; A(2,4) = 0.0d0; A(3,4) = 2.0d0; A(4,4) = 0.0d0
  A(1,5) = 1.0d0; A(2,5) = 2.0d0; A(3,5) = 0.0d0; A(4,5) = 1.0d0; A(5,5) = 5.0d0
  call DSYTRF('U', n, A, NMAX, IPIV, WORK_TF, LWORK_TF, INFO)
  call begin_test('5x5_upper_mixed')
  call print_int('info_trf', INFO)
  call print_array('a_factored', A, NMAX*n)
  call print_int_array('ipiv', IPIV, n)
  call DSYTRI2('U', n, A, NMAX, IPIV, WORK_2, LWORK_2, INFO)
  call print_int('info', INFO)
  call print_array('a_inv', A, NMAX*n)
  call end_test()

  ! Test 7: N=1 trivial
  n = 1
  A(1,1) = 5.0d0
  call DSYTRF('U', n, A, NMAX, IPIV, WORK_TF, LWORK_TF, INFO)
  call begin_test('n_one_upper')
  call print_int('info_trf', INFO)
  call print_array('a_factored', A, NMAX*n)
  call print_int_array('ipiv', IPIV, n)
  call DSYTRI2('U', n, A, NMAX, IPIV, WORK_2, LWORK_2, INFO)
  call print_int('info', INFO)
  call print_array('a_inv', A, NMAX*n)
  call end_test()

  ! Test 8: N=0 quick return
  n = 0
  call begin_test('n_zero')
  call DSYTRI2('L', n, A, NMAX, IPIV, WORK_2, LWORK_2, INFO)
  call print_int('info', INFO)
  call end_test()

  ! Test 9: 40x40 lower well-conditioned, exercises blocked path in JS (N > 32).
  ! Diagonally dominant symmetric pattern.
  n = 40
  A = 0.0d0
  do j = 1, n
    do i = j, n
      if ( i .eq. j ) then
        A(i, j) = 50.0d0 + dble(i)
      else
        A(i, j) = sin(0.7d0 * dble(i + 3*j))
      end if
    end do
  end do
  call DSYTRF('L', n, A, NMAX, IPIV, WORK_TF, LWORK_TF, INFO)
  call begin_test('40x40_lower_dd')
  call print_int('info_trf', INFO)
  call print_array('a_factored', A, NMAX*n)
  call print_int_array('ipiv', IPIV, n)
  call DSYTRI2('L', n, A, NMAX, IPIV, WORK_2, LWORK_2, INFO)
  call print_int('info', INFO)
  call print_array('a_inv', A, NMAX*n)
  call end_test()

  ! Test 10: 40x40 upper well-conditioned, exercises blocked path in JS.
  n = 40
  A = 0.0d0
  do j = 1, n
    do i = 1, j
      if ( i .eq. j ) then
        A(i, j) = 50.0d0 + dble(j)
      else
        A(i, j) = sin(0.7d0 * dble(j + 3*i))
      end if
    end do
  end do
  call DSYTRF('U', n, A, NMAX, IPIV, WORK_TF, LWORK_TF, INFO)
  call begin_test('40x40_upper_dd')
  call print_int('info_trf', INFO)
  call print_array('a_factored', A, NMAX*n)
  call print_int_array('ipiv', IPIV, n)
  call DSYTRI2('U', n, A, NMAX, IPIV, WORK_2, LWORK_2, INFO)
  call print_int('info', INFO)
  call print_array('a_inv', A, NMAX*n)
  call end_test()

end program
