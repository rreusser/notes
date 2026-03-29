program test_dtfttr
  use test_utils
  implicit none
  integer, parameter :: NMAX = 8
  double precision :: ARF(NMAX*(NMAX+1)/2), A(NMAX, NMAX)
  integer :: INFO, i, j, N, NT

  ! =============================================
  ! Edge cases
  ! =============================================

  ! Test 1: N=0 quick return
  A = -1.0d0
  call dtfttr('N', 'L', 0, ARF, A, 1, INFO)
  call begin_test('n0')
  call print_int('info', INFO)
  call end_test()

  ! Test 2: N=1 quick return
  ARF(1) = 42.0d0
  A = 0.0d0
  call dtfttr('N', 'L', 1, ARF, A, 1, INFO)
  call begin_test('n1')
  call print_int('info', INFO)
  call print_scalar('a00', A(1,1))
  call end_test()

  ! =============================================
  ! N=5 (odd) — all 4 combinations
  ! =============================================
  N = 5
  NT = N*(N+1)/2  ! = 15

  ! Fill ARF with sequential values
  do i = 1, NT
    ARF(i) = dble(i)
  end do

  ! Test 3: N=5, TRANSR='N', UPLO='L' (odd, normal, lower)
  A = 0.0d0
  call dtfttr('N', 'L', N, ARF, A, N, INFO)
  call begin_test('n5_N_L')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_array('ARF', ARF, NT)
  call print_matrix('A', A, N, N, N)
  call end_test()

  ! Test 4: N=5, TRANSR='N', UPLO='U' (odd, normal, upper)
  A = 0.0d0
  call dtfttr('N', 'U', N, ARF, A, N, INFO)
  call begin_test('n5_N_U')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_array('ARF', ARF, NT)
  call print_matrix('A', A, N, N, N)
  call end_test()

  ! Test 5: N=5, TRANSR='T', UPLO='L' (odd, transpose, lower)
  A = 0.0d0
  call dtfttr('T', 'L', N, ARF, A, N, INFO)
  call begin_test('n5_T_L')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_array('ARF', ARF, NT)
  call print_matrix('A', A, N, N, N)
  call end_test()

  ! Test 6: N=5, TRANSR='T', UPLO='U' (odd, transpose, upper)
  A = 0.0d0
  call dtfttr('T', 'U', N, ARF, A, N, INFO)
  call begin_test('n5_T_U')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_array('ARF', ARF, NT)
  call print_matrix('A', A, N, N, N)
  call end_test()

  ! =============================================
  ! N=6 (even) — all 4 combinations
  ! =============================================
  N = 6
  NT = N*(N+1)/2  ! = 21

  ! Fill ARF with sequential values
  do i = 1, NT
    ARF(i) = dble(i)
  end do

  ! Test 7: N=6, TRANSR='N', UPLO='L' (even, normal, lower)
  A = 0.0d0
  call dtfttr('N', 'L', N, ARF, A, N, INFO)
  call begin_test('n6_N_L')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_array('ARF', ARF, NT)
  call print_matrix('A', A, N, N, N)
  call end_test()

  ! Test 8: N=6, TRANSR='N', UPLO='U' (even, normal, upper)
  A = 0.0d0
  call dtfttr('N', 'U', N, ARF, A, N, INFO)
  call begin_test('n6_N_U')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_array('ARF', ARF, NT)
  call print_matrix('A', A, N, N, N)
  call end_test()

  ! Test 9: N=6, TRANSR='T', UPLO='L' (even, transpose, lower)
  A = 0.0d0
  call dtfttr('T', 'L', N, ARF, A, N, INFO)
  call begin_test('n6_T_L')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_array('ARF', ARF, NT)
  call print_matrix('A', A, N, N, N)
  call end_test()

  ! Test 10: N=6, TRANSR='T', UPLO='U' (even, transpose, upper)
  A = 0.0d0
  call dtfttr('T', 'U', N, ARF, A, N, INFO)
  call begin_test('n6_T_U')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_array('ARF', ARF, NT)
  call print_matrix('A', A, N, N, N)
  call end_test()

  ! =============================================
  ! N=7 (odd, larger) — all 4 combinations
  ! =============================================
  N = 7
  NT = N*(N+1)/2  ! = 28

  ! Fill ARF with distinct values
  do i = 1, NT
    ARF(i) = dble(i) * 0.1d0
  end do

  ! Test 11: N=7, TRANSR='N', UPLO='L' (odd, normal, lower)
  A = 0.0d0
  call dtfttr('N', 'L', N, ARF, A, N, INFO)
  call begin_test('n7_N_L')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_array('ARF', ARF, NT)
  call print_matrix('A', A, N, N, N)
  call end_test()

  ! Test 12: N=7, TRANSR='N', UPLO='U' (odd, normal, upper)
  A = 0.0d0
  call dtfttr('N', 'U', N, ARF, A, N, INFO)
  call begin_test('n7_N_U')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_array('ARF', ARF, NT)
  call print_matrix('A', A, N, N, N)
  call end_test()

  ! Test 13: N=7, TRANSR='T', UPLO='L' (odd, transpose, lower)
  A = 0.0d0
  call dtfttr('T', 'L', N, ARF, A, N, INFO)
  call begin_test('n7_T_L')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_array('ARF', ARF, NT)
  call print_matrix('A', A, N, N, N)
  call end_test()

  ! Test 14: N=7, TRANSR='T', UPLO='U' (odd, transpose, upper)
  A = 0.0d0
  call dtfttr('T', 'U', N, ARF, A, N, INFO)
  call begin_test('n7_T_U')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_array('ARF', ARF, NT)
  call print_matrix('A', A, N, N, N)
  call end_test()

  ! =============================================
  ! N=8 (even, larger) — all 4 combinations
  ! =============================================
  N = 8
  NT = N*(N+1)/2  ! = 36

  ! Fill ARF with distinct values
  do i = 1, NT
    ARF(i) = dble(i) * 0.5d0
  end do

  ! Test 15: N=8, TRANSR='N', UPLO='L' (even, normal, lower)
  A = 0.0d0
  call dtfttr('N', 'L', N, ARF, A, N, INFO)
  call begin_test('n8_N_L')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_array('ARF', ARF, NT)
  call print_matrix('A', A, N, N, N)
  call end_test()

  ! Test 16: N=8, TRANSR='N', UPLO='U' (even, normal, upper)
  A = 0.0d0
  call dtfttr('N', 'U', N, ARF, A, N, INFO)
  call begin_test('n8_N_U')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_array('ARF', ARF, NT)
  call print_matrix('A', A, N, N, N)
  call end_test()

  ! Test 17: N=8, TRANSR='T', UPLO='L' (even, transpose, lower)
  A = 0.0d0
  call dtfttr('T', 'L', N, ARF, A, N, INFO)
  call begin_test('n8_T_L')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_array('ARF', ARF, NT)
  call print_matrix('A', A, N, N, N)
  call end_test()

  ! Test 18: N=8, TRANSR='T', UPLO='U' (even, transpose, upper)
  A = 0.0d0
  call dtfttr('T', 'U', N, ARF, A, N, INFO)
  call begin_test('n8_T_U')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_array('ARF', ARF, NT)
  call print_matrix('A', A, N, N, N)
  call end_test()

  ! =============================================
  ! LDA > N test
  ! =============================================

  ! Test 19: N=5, LDA=8, TRANSR='N', UPLO='L'
  N = 5
  NT = N*(N+1)/2
  do i = 1, NT
    ARF(i) = dble(i)
  end do
  A = 0.0d0
  call dtfttr('N', 'L', N, ARF, A, NMAX, INFO)
  call begin_test('n5_N_L_lda8')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_int('lda', NMAX)
  call print_array('ARF', ARF, NT)
  call print_matrix('A', A, NMAX, N, N)
  call end_test()

end program
