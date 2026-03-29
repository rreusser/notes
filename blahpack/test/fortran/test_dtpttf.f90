program test_dtpttf
  use test_utils
  implicit none
  integer, parameter :: NMAX = 8
  double precision :: AP(NMAX*(NMAX+1)/2), ARF(NMAX*(NMAX+1)/2)
  integer :: INFO, i, N, NT

  ! =============================================
  ! Edge cases
  ! =============================================

  ! Test 1: N=0 quick return
  ARF = -1.0d0
  call dtpttf('N', 'L', 0, AP, ARF, INFO)
  call begin_test('n0')
  call print_int('info', INFO)
  call end_test()

  ! Test 2: N=1
  AP(1) = 42.0d0
  ARF = 0.0d0
  call dtpttf('N', 'L', 1, AP, ARF, INFO)
  call begin_test('n1_N_L')
  call print_int('info', INFO)
  call print_scalar('arf0', ARF(1))
  call end_test()

  ! Test 3: N=1, transpose
  AP(1) = 99.0d0
  ARF = 0.0d0
  call dtpttf('T', 'U', 1, AP, ARF, INFO)
  call begin_test('n1_T_U')
  call print_int('info', INFO)
  call print_scalar('arf0', ARF(1))
  call end_test()

  ! =============================================
  ! N=5 (odd) — all 4 combinations
  ! =============================================
  N = 5
  NT = N*(N+1)/2  ! = 15

  ! Fill AP with sequential values
  do i = 1, NT
    AP(i) = dble(i)
  end do

  ! Test 4: N=5, TRANSR='N', UPLO='L' (odd, normal, lower)
  ARF = 0.0d0
  call dtpttf('N', 'L', N, AP, ARF, INFO)
  call begin_test('n5_N_L')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_array('AP', AP, NT)
  call print_array('ARF', ARF, NT)
  call end_test()

  ! Test 5: N=5, TRANSR='N', UPLO='U' (odd, normal, upper)
  ARF = 0.0d0
  call dtpttf('N', 'U', N, AP, ARF, INFO)
  call begin_test('n5_N_U')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_array('AP', AP, NT)
  call print_array('ARF', ARF, NT)
  call end_test()

  ! Test 6: N=5, TRANSR='T', UPLO='L' (odd, transpose, lower)
  ARF = 0.0d0
  call dtpttf('T', 'L', N, AP, ARF, INFO)
  call begin_test('n5_T_L')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_array('AP', AP, NT)
  call print_array('ARF', ARF, NT)
  call end_test()

  ! Test 7: N=5, TRANSR='T', UPLO='U' (odd, transpose, upper)
  ARF = 0.0d0
  call dtpttf('T', 'U', N, AP, ARF, INFO)
  call begin_test('n5_T_U')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_array('AP', AP, NT)
  call print_array('ARF', ARF, NT)
  call end_test()

  ! =============================================
  ! N=6 (even) — all 4 combinations
  ! =============================================
  N = 6
  NT = N*(N+1)/2  ! = 21

  ! Fill AP with sequential values
  do i = 1, NT
    AP(i) = dble(i)
  end do

  ! Test 8: N=6, TRANSR='N', UPLO='L' (even, normal, lower)
  ARF = 0.0d0
  call dtpttf('N', 'L', N, AP, ARF, INFO)
  call begin_test('n6_N_L')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_array('AP', AP, NT)
  call print_array('ARF', ARF, NT)
  call end_test()

  ! Test 9: N=6, TRANSR='N', UPLO='U' (even, normal, upper)
  ARF = 0.0d0
  call dtpttf('N', 'U', N, AP, ARF, INFO)
  call begin_test('n6_N_U')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_array('AP', AP, NT)
  call print_array('ARF', ARF, NT)
  call end_test()

  ! Test 10: N=6, TRANSR='T', UPLO='L' (even, transpose, lower)
  ARF = 0.0d0
  call dtpttf('T', 'L', N, AP, ARF, INFO)
  call begin_test('n6_T_L')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_array('AP', AP, NT)
  call print_array('ARF', ARF, NT)
  call end_test()

  ! Test 11: N=6, TRANSR='T', UPLO='U' (even, transpose, upper)
  ARF = 0.0d0
  call dtpttf('T', 'U', N, AP, ARF, INFO)
  call begin_test('n6_T_U')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_array('AP', AP, NT)
  call print_array('ARF', ARF, NT)
  call end_test()

  ! =============================================
  ! N=7 (odd, larger) — all 4 combinations
  ! =============================================
  N = 7
  NT = N*(N+1)/2  ! = 28

  ! Fill AP with distinct values
  do i = 1, NT
    AP(i) = dble(i) * 0.1d0
  end do

  ! Test 12: N=7, TRANSR='N', UPLO='L'
  ARF = 0.0d0
  call dtpttf('N', 'L', N, AP, ARF, INFO)
  call begin_test('n7_N_L')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_array('AP', AP, NT)
  call print_array('ARF', ARF, NT)
  call end_test()

  ! Test 13: N=7, TRANSR='N', UPLO='U'
  ARF = 0.0d0
  call dtpttf('N', 'U', N, AP, ARF, INFO)
  call begin_test('n7_N_U')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_array('AP', AP, NT)
  call print_array('ARF', ARF, NT)
  call end_test()

  ! Test 14: N=7, TRANSR='T', UPLO='L'
  ARF = 0.0d0
  call dtpttf('T', 'L', N, AP, ARF, INFO)
  call begin_test('n7_T_L')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_array('AP', AP, NT)
  call print_array('ARF', ARF, NT)
  call end_test()

  ! Test 15: N=7, TRANSR='T', UPLO='U'
  ARF = 0.0d0
  call dtpttf('T', 'U', N, AP, ARF, INFO)
  call begin_test('n7_T_U')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_array('AP', AP, NT)
  call print_array('ARF', ARF, NT)
  call end_test()

  ! =============================================
  ! N=8 (even, larger) — all 4 combinations
  ! =============================================
  N = 8
  NT = N*(N+1)/2  ! = 36

  ! Fill AP with distinct values
  do i = 1, NT
    AP(i) = dble(i) * 0.5d0
  end do

  ! Test 16: N=8, TRANSR='N', UPLO='L'
  ARF = 0.0d0
  call dtpttf('N', 'L', N, AP, ARF, INFO)
  call begin_test('n8_N_L')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_array('AP', AP, NT)
  call print_array('ARF', ARF, NT)
  call end_test()

  ! Test 17: N=8, TRANSR='N', UPLO='U'
  ARF = 0.0d0
  call dtpttf('N', 'U', N, AP, ARF, INFO)
  call begin_test('n8_N_U')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_array('AP', AP, NT)
  call print_array('ARF', ARF, NT)
  call end_test()

  ! Test 18: N=8, TRANSR='T', UPLO='L'
  ARF = 0.0d0
  call dtpttf('T', 'L', N, AP, ARF, INFO)
  call begin_test('n8_T_L')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_array('AP', AP, NT)
  call print_array('ARF', ARF, NT)
  call end_test()

  ! Test 19: N=8, TRANSR='T', UPLO='U'
  ARF = 0.0d0
  call dtpttf('T', 'U', N, AP, ARF, INFO)
  call begin_test('n8_T_U')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_array('AP', AP, NT)
  call print_array('ARF', ARF, NT)
  call end_test()

end program
