program test_dtrttf
  use test_utils
  implicit none
  integer, parameter :: NMAX = 8
  double precision :: A(NMAX, NMAX), ARF(NMAX*(NMAX+1)/2)
  integer :: INFO, i, j, N, NT

  ! =============================================
  ! Edge cases
  ! =============================================

  ! Test 1: N=0 quick return
  ARF = -1.0d0
  call dtrttf('N', 'L', 0, A, 1, ARF, INFO)
  call begin_test('n0')
  call print_int('info', INFO)
  call end_test()

  ! Test 2: N=1 quick return
  A(1,1) = 42.0d0
  ARF = 0.0d0
  call dtrttf('N', 'L', 1, A, 1, ARF, INFO)
  call begin_test('n1')
  call print_int('info', INFO)
  call print_scalar('arf0', ARF(1))
  call end_test()

  ! =============================================
  ! N=5 (odd) - all 4 combinations
  ! =============================================
  N = 5
  NT = N*(N+1)/2  ! = 15

  ! Fill A lower triangle with sequential values
  A = 0.0d0
  do j = 1, N
    do i = j, N
      A(i, j) = dble(i + (j-1)*N)
    end do
  end do

  ! Test 3: N=5, TRANSR='N', UPLO='L' (odd, normal, lower)
  ARF = 0.0d0
  call dtrttf('N', 'L', N, A, N, ARF, INFO)
  call begin_test('n5_N_L')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_matrix('A', A, N, N, N)
  call print_array('ARF', ARF, NT)
  call end_test()

  ! Test 4: N=5, TRANSR='T', UPLO='L' (odd, transpose, lower)
  ARF = 0.0d0
  call dtrttf('T', 'L', N, A, N, ARF, INFO)
  call begin_test('n5_T_L')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_matrix('A', A, N, N, N)
  call print_array('ARF', ARF, NT)
  call end_test()

  ! Fill A upper triangle with sequential values
  A = 0.0d0
  do j = 1, N
    do i = 1, j
      A(i, j) = dble(i + (j-1)*N)
    end do
  end do

  ! Test 5: N=5, TRANSR='N', UPLO='U' (odd, normal, upper)
  ARF = 0.0d0
  call dtrttf('N', 'U', N, A, N, ARF, INFO)
  call begin_test('n5_N_U')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_matrix('A', A, N, N, N)
  call print_array('ARF', ARF, NT)
  call end_test()

  ! Test 6: N=5, TRANSR='T', UPLO='U' (odd, transpose, upper)
  ARF = 0.0d0
  call dtrttf('T', 'U', N, A, N, ARF, INFO)
  call begin_test('n5_T_U')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_matrix('A', A, N, N, N)
  call print_array('ARF', ARF, NT)
  call end_test()

  ! =============================================
  ! N=6 (even) - all 4 combinations
  ! =============================================
  N = 6
  NT = N*(N+1)/2  ! = 21

  ! Fill A lower triangle with sequential values
  A = 0.0d0
  do j = 1, N
    do i = j, N
      A(i, j) = dble(i + (j-1)*N)
    end do
  end do

  ! Test 7: N=6, TRANSR='N', UPLO='L' (even, normal, lower)
  ARF = 0.0d0
  call dtrttf('N', 'L', N, A, N, ARF, INFO)
  call begin_test('n6_N_L')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_matrix('A', A, N, N, N)
  call print_array('ARF', ARF, NT)
  call end_test()

  ! Test 8: N=6, TRANSR='T', UPLO='L' (even, transpose, lower)
  ARF = 0.0d0
  call dtrttf('T', 'L', N, A, N, ARF, INFO)
  call begin_test('n6_T_L')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_matrix('A', A, N, N, N)
  call print_array('ARF', ARF, NT)
  call end_test()

  ! Fill A upper triangle with sequential values
  A = 0.0d0
  do j = 1, N
    do i = 1, j
      A(i, j) = dble(i + (j-1)*N)
    end do
  end do

  ! Test 9: N=6, TRANSR='N', UPLO='U' (even, normal, upper)
  ARF = 0.0d0
  call dtrttf('N', 'U', N, A, N, ARF, INFO)
  call begin_test('n6_N_U')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_matrix('A', A, N, N, N)
  call print_array('ARF', ARF, NT)
  call end_test()

  ! Test 10: N=6, TRANSR='T', UPLO='U' (even, transpose, upper)
  ARF = 0.0d0
  call dtrttf('T', 'U', N, A, N, ARF, INFO)
  call begin_test('n6_T_U')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_matrix('A', A, N, N, N)
  call print_array('ARF', ARF, NT)
  call end_test()

  ! =============================================
  ! Round-trip test: dtrttf then dtfttr
  ! =============================================
  N = 5
  NT = N*(N+1)/2

  ! Fill A lower triangle with distinct values
  A = 0.0d0
  do j = 1, N
    do i = j, N
      A(i, j) = dble(10*i + j)
    end do
  end do

  ! Test 11: Round-trip lower, normal
  ARF = 0.0d0
  call dtrttf('N', 'L', N, A, N, ARF, INFO)
  call begin_test('roundtrip_N_L')
  call print_int('info', INFO)
  call print_array('ARF', ARF, NT)
  call end_test()

end program
