program test_ztrttf
  use test_utils
  implicit none
  integer, parameter :: NMAX = 8
  integer, parameter :: ARFMAX = NMAX*(NMAX+1)/2
  complex*16 :: A(NMAX, NMAX), ARF(ARFMAX)
  double precision :: ARF_r(2*ARFMAX)
  double precision :: A_pk_r(2*NMAX*NMAX)
  complex*16 :: A_pk(NMAX*NMAX)
  equivalence (ARF, ARF_r)
  equivalence (A_pk, A_pk_r)
  integer :: INFO, i, j, N, NT, ii

  ! =============================================
  ! Edge cases
  ! =============================================

  ! Test 1: N=0 quick return
  call ZTRTTF('N', 'L', 0, A, 1, ARF, INFO)
  call begin_test('n0')
  call print_int('info', INFO)
  call end_test()

  ! Test 2: N=1, TRANSR='N' quick return
  A(1,1) = dcmplx(3.0d0, 4.0d0)
  call ZTRTTF('N', 'L', 1, A, NMAX, ARF, INFO)
  call begin_test('n1_N')
  call print_int('info', INFO)
  call print_array('ARF', ARF_r, 2)
  call end_test()

  ! Test 3: N=1, TRANSR='C' quick return (conjugate)
  A(1,1) = dcmplx(3.0d0, 4.0d0)
  call ZTRTTF('C', 'L', 1, A, NMAX, ARF, INFO)
  call begin_test('n1_C')
  call print_int('info', INFO)
  call print_array('ARF', ARF_r, 2)
  call end_test()

  ! =============================================
  ! N=5 (odd) — all 4 combinations
  ! =============================================
  N = 5
  NT = N*(N+1)/2  ! = 15

  ! Fill A with distinct complex values
  do j = 1, N
    do i = 1, N
      A(i, j) = dcmplx(dble(i + (j-1)*N), dble(i + (j-1)*N) * 0.1d0)
    end do
  end do

  ! Test 4: N=5, TRANSR='N', UPLO='L' (odd, normal, lower)
  do ii = 1, ARFMAX
    ARF(ii) = dcmplx(0.0d0, 0.0d0)
  end do
  call ZTRTTF('N', 'L', N, A, NMAX, ARF, INFO)
  call begin_test('n5_N_L')
  call print_int('info', INFO)
  call print_int('n', N)
  do j = 1, N
    do i = 1, N
      A_pk(i + (j-1)*N) = A(i, j)
    end do
  end do
  call print_array('A', A_pk_r, 2*N*N)
  call print_array('ARF', ARF_r, 2*NT)
  call end_test()

  ! Test 5: N=5, TRANSR='N', UPLO='U' (odd, normal, upper)
  do ii = 1, ARFMAX
    ARF(ii) = dcmplx(0.0d0, 0.0d0)
  end do
  call ZTRTTF('N', 'U', N, A, NMAX, ARF, INFO)
  call begin_test('n5_N_U')
  call print_int('info', INFO)
  call print_int('n', N)
  do j = 1, N
    do i = 1, N
      A_pk(i + (j-1)*N) = A(i, j)
    end do
  end do
  call print_array('A', A_pk_r, 2*N*N)
  call print_array('ARF', ARF_r, 2*NT)
  call end_test()

  ! Test 6: N=5, TRANSR='C', UPLO='L' (odd, conj-transpose, lower)
  do ii = 1, ARFMAX
    ARF(ii) = dcmplx(0.0d0, 0.0d0)
  end do
  call ZTRTTF('C', 'L', N, A, NMAX, ARF, INFO)
  call begin_test('n5_C_L')
  call print_int('info', INFO)
  call print_int('n', N)
  do j = 1, N
    do i = 1, N
      A_pk(i + (j-1)*N) = A(i, j)
    end do
  end do
  call print_array('A', A_pk_r, 2*N*N)
  call print_array('ARF', ARF_r, 2*NT)
  call end_test()

  ! Test 7: N=5, TRANSR='C', UPLO='U' (odd, conj-transpose, upper)
  do ii = 1, ARFMAX
    ARF(ii) = dcmplx(0.0d0, 0.0d0)
  end do
  call ZTRTTF('C', 'U', N, A, NMAX, ARF, INFO)
  call begin_test('n5_C_U')
  call print_int('info', INFO)
  call print_int('n', N)
  do j = 1, N
    do i = 1, N
      A_pk(i + (j-1)*N) = A(i, j)
    end do
  end do
  call print_array('A', A_pk_r, 2*N*N)
  call print_array('ARF', ARF_r, 2*NT)
  call end_test()

  ! =============================================
  ! N=6 (even) — all 4 combinations
  ! =============================================
  N = 6
  NT = N*(N+1)/2  ! = 21

  ! Fill A with distinct complex values
  do j = 1, N
    do i = 1, N
      A(i, j) = dcmplx(dble(i + (j-1)*N) * 0.5d0, dble(i + (j-1)*N) * 0.3d0)
    end do
  end do

  ! Test 8: N=6, TRANSR='N', UPLO='L' (even, normal, lower)
  do ii = 1, ARFMAX
    ARF(ii) = dcmplx(0.0d0, 0.0d0)
  end do
  call ZTRTTF('N', 'L', N, A, NMAX, ARF, INFO)
  call begin_test('n6_N_L')
  call print_int('info', INFO)
  call print_int('n', N)
  do j = 1, N
    do i = 1, N
      A_pk(i + (j-1)*N) = A(i, j)
    end do
  end do
  call print_array('A', A_pk_r, 2*N*N)
  call print_array('ARF', ARF_r, 2*NT)
  call end_test()

  ! Test 9: N=6, TRANSR='N', UPLO='U' (even, normal, upper)
  do ii = 1, ARFMAX
    ARF(ii) = dcmplx(0.0d0, 0.0d0)
  end do
  call ZTRTTF('N', 'U', N, A, NMAX, ARF, INFO)
  call begin_test('n6_N_U')
  call print_int('info', INFO)
  call print_int('n', N)
  do j = 1, N
    do i = 1, N
      A_pk(i + (j-1)*N) = A(i, j)
    end do
  end do
  call print_array('A', A_pk_r, 2*N*N)
  call print_array('ARF', ARF_r, 2*NT)
  call end_test()

  ! Test 10: N=6, TRANSR='C', UPLO='L' (even, conj-transpose, lower)
  do ii = 1, ARFMAX
    ARF(ii) = dcmplx(0.0d0, 0.0d0)
  end do
  call ZTRTTF('C', 'L', N, A, NMAX, ARF, INFO)
  call begin_test('n6_C_L')
  call print_int('info', INFO)
  call print_int('n', N)
  do j = 1, N
    do i = 1, N
      A_pk(i + (j-1)*N) = A(i, j)
    end do
  end do
  call print_array('A', A_pk_r, 2*N*N)
  call print_array('ARF', ARF_r, 2*NT)
  call end_test()

  ! Test 11: N=6, TRANSR='C', UPLO='U' (even, conj-transpose, upper)
  do ii = 1, ARFMAX
    ARF(ii) = dcmplx(0.0d0, 0.0d0)
  end do
  call ZTRTTF('C', 'U', N, A, NMAX, ARF, INFO)
  call begin_test('n6_C_U')
  call print_int('info', INFO)
  call print_int('n', N)
  do j = 1, N
    do i = 1, N
      A_pk(i + (j-1)*N) = A(i, j)
    end do
  end do
  call print_array('A', A_pk_r, 2*N*N)
  call print_array('ARF', ARF_r, 2*NT)
  call end_test()

  ! =============================================
  ! N=7 (odd, larger) — all 4 combinations
  ! =============================================
  N = 7
  NT = N*(N+1)/2  ! = 28

  ! Fill A with distinct complex values
  do j = 1, N
    do i = 1, N
      A(i, j) = dcmplx(dble(i + (j-1)*N) * 0.1d0, dble(i + (j-1)*N) * (-0.2d0))
    end do
  end do

  ! Test 12: N=7, TRANSR='N', UPLO='L' (odd, normal, lower)
  do ii = 1, ARFMAX
    ARF(ii) = dcmplx(0.0d0, 0.0d0)
  end do
  call ZTRTTF('N', 'L', N, A, NMAX, ARF, INFO)
  call begin_test('n7_N_L')
  call print_int('info', INFO)
  call print_int('n', N)
  do j = 1, N
    do i = 1, N
      A_pk(i + (j-1)*N) = A(i, j)
    end do
  end do
  call print_array('A', A_pk_r, 2*N*N)
  call print_array('ARF', ARF_r, 2*NT)
  call end_test()

  ! Test 13: N=7, TRANSR='N', UPLO='U' (odd, normal, upper)
  do ii = 1, ARFMAX
    ARF(ii) = dcmplx(0.0d0, 0.0d0)
  end do
  call ZTRTTF('N', 'U', N, A, NMAX, ARF, INFO)
  call begin_test('n7_N_U')
  call print_int('info', INFO)
  call print_int('n', N)
  do j = 1, N
    do i = 1, N
      A_pk(i + (j-1)*N) = A(i, j)
    end do
  end do
  call print_array('A', A_pk_r, 2*N*N)
  call print_array('ARF', ARF_r, 2*NT)
  call end_test()

  ! Test 14: N=7, TRANSR='C', UPLO='L' (odd, conj-transpose, lower)
  do ii = 1, ARFMAX
    ARF(ii) = dcmplx(0.0d0, 0.0d0)
  end do
  call ZTRTTF('C', 'L', N, A, NMAX, ARF, INFO)
  call begin_test('n7_C_L')
  call print_int('info', INFO)
  call print_int('n', N)
  do j = 1, N
    do i = 1, N
      A_pk(i + (j-1)*N) = A(i, j)
    end do
  end do
  call print_array('A', A_pk_r, 2*N*N)
  call print_array('ARF', ARF_r, 2*NT)
  call end_test()

  ! Test 15: N=7, TRANSR='C', UPLO='U' (odd, conj-transpose, upper)
  do ii = 1, ARFMAX
    ARF(ii) = dcmplx(0.0d0, 0.0d0)
  end do
  call ZTRTTF('C', 'U', N, A, NMAX, ARF, INFO)
  call begin_test('n7_C_U')
  call print_int('info', INFO)
  call print_int('n', N)
  do j = 1, N
    do i = 1, N
      A_pk(i + (j-1)*N) = A(i, j)
    end do
  end do
  call print_array('A', A_pk_r, 2*N*N)
  call print_array('ARF', ARF_r, 2*NT)
  call end_test()

  ! =============================================
  ! N=8 (even, matches NMAX) — all 4 combinations
  ! =============================================
  N = 8
  NT = N*(N+1)/2  ! = 36

  ! Fill A with distinct complex values
  do j = 1, N
    do i = 1, N
      A(i, j) = dcmplx(dble(i + (j-1)*N) * 0.25d0, dble(i + (j-1)*N) * (-0.15d0))
    end do
  end do

  ! Test 16: N=8, TRANSR='N', UPLO='L' (even, normal, lower)
  do ii = 1, ARFMAX
    ARF(ii) = dcmplx(0.0d0, 0.0d0)
  end do
  call ZTRTTF('N', 'L', N, A, NMAX, ARF, INFO)
  call begin_test('n8_N_L')
  call print_int('info', INFO)
  call print_int('n', N)
  do j = 1, N
    do i = 1, N
      A_pk(i + (j-1)*N) = A(i, j)
    end do
  end do
  call print_array('A', A_pk_r, 2*N*N)
  call print_array('ARF', ARF_r, 2*NT)
  call end_test()

  ! Test 17: N=8, TRANSR='N', UPLO='U' (even, normal, upper)
  do ii = 1, ARFMAX
    ARF(ii) = dcmplx(0.0d0, 0.0d0)
  end do
  call ZTRTTF('N', 'U', N, A, NMAX, ARF, INFO)
  call begin_test('n8_N_U')
  call print_int('info', INFO)
  call print_int('n', N)
  do j = 1, N
    do i = 1, N
      A_pk(i + (j-1)*N) = A(i, j)
    end do
  end do
  call print_array('A', A_pk_r, 2*N*N)
  call print_array('ARF', ARF_r, 2*NT)
  call end_test()

  ! Test 18: N=8, TRANSR='C', UPLO='L' (even, conj-transpose, lower)
  do ii = 1, ARFMAX
    ARF(ii) = dcmplx(0.0d0, 0.0d0)
  end do
  call ZTRTTF('C', 'L', N, A, NMAX, ARF, INFO)
  call begin_test('n8_C_L')
  call print_int('info', INFO)
  call print_int('n', N)
  do j = 1, N
    do i = 1, N
      A_pk(i + (j-1)*N) = A(i, j)
    end do
  end do
  call print_array('A', A_pk_r, 2*N*N)
  call print_array('ARF', ARF_r, 2*NT)
  call end_test()

  ! Test 19: N=8, TRANSR='C', UPLO='U' (even, conj-transpose, upper)
  do ii = 1, ARFMAX
    ARF(ii) = dcmplx(0.0d0, 0.0d0)
  end do
  call ZTRTTF('C', 'U', N, A, NMAX, ARF, INFO)
  call begin_test('n8_C_U')
  call print_int('info', INFO)
  call print_int('n', N)
  do j = 1, N
    do i = 1, N
      A_pk(i + (j-1)*N) = A(i, j)
    end do
  end do
  call print_array('A', A_pk_r, 2*N*N)
  call print_array('ARF', ARF_r, 2*NT)
  call end_test()

end program
