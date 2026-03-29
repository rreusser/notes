program test_ztpttf
  use test_utils
  implicit none
  integer, parameter :: NMAX = 8
  integer, parameter :: NTMAX = NMAX*(NMAX+1)/2
  complex*16 :: AP(NTMAX), ARF(NTMAX)
  double precision :: AP_r(2*NTMAX), ARF_r(2*NTMAX)
  equivalence (AP, AP_r)
  equivalence (ARF, ARF_r)
  integer :: INFO, i, N, NT
  double precision :: rval, ival

  ! =============================================
  ! Edge cases
  ! =============================================

  ! Test 1: N=0 quick return
  ARF = (0.0d0, 0.0d0)
  call ztpttf('N', 'L', 0, AP, ARF, INFO)
  call begin_test('n0')
  call print_int('info', INFO)
  call end_test()

  ! Test 2: N=1, normal, lower
  AP(1) = (42.0d0, 7.0d0)
  ARF = (0.0d0, 0.0d0)
  call ztpttf('N', 'L', 1, AP, ARF, INFO)
  call begin_test('n1_N_L')
  call print_int('info', INFO)
  call print_array('ARF', ARF_r, 2)
  call end_test()

  ! Test 3: N=1, conjugate-transpose, upper
  AP(1) = (99.0d0, -3.0d0)
  ARF = (0.0d0, 0.0d0)
  call ztpttf('C', 'U', 1, AP, ARF, INFO)
  call begin_test('n1_C_U')
  call print_int('info', INFO)
  call print_array('ARF', ARF_r, 2)
  call end_test()

  ! =============================================
  ! N=5 (odd) — all 4 combinations
  ! =============================================
  N = 5
  NT = N*(N+1)/2  ! = 15

  ! Fill AP with distinct complex values
  do i = 1, NT
    rval = dble(i)
    ival = dble(i) * 0.1d0
    AP(i) = dcmplx(rval, ival)
  end do

  ! Test 4: N=5, TRANSR='N', UPLO='L' (odd, normal, lower)
  ARF = (0.0d0, 0.0d0)
  call ztpttf('N', 'L', N, AP, ARF, INFO)
  call begin_test('n5_N_L')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_array('AP', AP_r, 2*NT)
  call print_array('ARF', ARF_r, 2*NT)
  call end_test()

  ! Test 5: N=5, TRANSR='N', UPLO='U' (odd, normal, upper)
  ARF = (0.0d0, 0.0d0)
  call ztpttf('N', 'U', N, AP, ARF, INFO)
  call begin_test('n5_N_U')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_array('AP', AP_r, 2*NT)
  call print_array('ARF', ARF_r, 2*NT)
  call end_test()

  ! Test 6: N=5, TRANSR='C', UPLO='L' (odd, conj-transpose, lower)
  ARF = (0.0d0, 0.0d0)
  call ztpttf('C', 'L', N, AP, ARF, INFO)
  call begin_test('n5_C_L')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_array('AP', AP_r, 2*NT)
  call print_array('ARF', ARF_r, 2*NT)
  call end_test()

  ! Test 7: N=5, TRANSR='C', UPLO='U' (odd, conj-transpose, upper)
  ARF = (0.0d0, 0.0d0)
  call ztpttf('C', 'U', N, AP, ARF, INFO)
  call begin_test('n5_C_U')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_array('AP', AP_r, 2*NT)
  call print_array('ARF', ARF_r, 2*NT)
  call end_test()

  ! =============================================
  ! N=6 (even) — all 4 combinations
  ! =============================================
  N = 6
  NT = N*(N+1)/2  ! = 21

  ! Fill AP with distinct complex values
  do i = 1, NT
    rval = dble(i)
    ival = dble(i) * 0.1d0
    AP(i) = dcmplx(rval, ival)
  end do

  ! Test 8: N=6, TRANSR='N', UPLO='L' (even, normal, lower)
  ARF = (0.0d0, 0.0d0)
  call ztpttf('N', 'L', N, AP, ARF, INFO)
  call begin_test('n6_N_L')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_array('AP', AP_r, 2*NT)
  call print_array('ARF', ARF_r, 2*NT)
  call end_test()

  ! Test 9: N=6, TRANSR='N', UPLO='U' (even, normal, upper)
  ARF = (0.0d0, 0.0d0)
  call ztpttf('N', 'U', N, AP, ARF, INFO)
  call begin_test('n6_N_U')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_array('AP', AP_r, 2*NT)
  call print_array('ARF', ARF_r, 2*NT)
  call end_test()

  ! Test 10: N=6, TRANSR='C', UPLO='L' (even, conj-transpose, lower)
  ARF = (0.0d0, 0.0d0)
  call ztpttf('C', 'L', N, AP, ARF, INFO)
  call begin_test('n6_C_L')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_array('AP', AP_r, 2*NT)
  call print_array('ARF', ARF_r, 2*NT)
  call end_test()

  ! Test 11: N=6, TRANSR='C', UPLO='U' (even, conj-transpose, upper)
  ARF = (0.0d0, 0.0d0)
  call ztpttf('C', 'U', N, AP, ARF, INFO)
  call begin_test('n6_C_U')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_array('AP', AP_r, 2*NT)
  call print_array('ARF', ARF_r, 2*NT)
  call end_test()

  ! =============================================
  ! N=7 (odd, larger) — all 4 combinations
  ! =============================================
  N = 7
  NT = N*(N+1)/2  ! = 28

  ! Fill AP with distinct complex values
  do i = 1, NT
    rval = dble(i) * 0.1d0
    ival = dble(i) * 0.3d0
    AP(i) = dcmplx(rval, ival)
  end do

  ! Test 12: N=7, TRANSR='N', UPLO='L'
  ARF = (0.0d0, 0.0d0)
  call ztpttf('N', 'L', N, AP, ARF, INFO)
  call begin_test('n7_N_L')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_array('AP', AP_r, 2*NT)
  call print_array('ARF', ARF_r, 2*NT)
  call end_test()

  ! Test 13: N=7, TRANSR='N', UPLO='U'
  ARF = (0.0d0, 0.0d0)
  call ztpttf('N', 'U', N, AP, ARF, INFO)
  call begin_test('n7_N_U')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_array('AP', AP_r, 2*NT)
  call print_array('ARF', ARF_r, 2*NT)
  call end_test()

  ! Test 14: N=7, TRANSR='C', UPLO='L'
  ARF = (0.0d0, 0.0d0)
  call ztpttf('C', 'L', N, AP, ARF, INFO)
  call begin_test('n7_C_L')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_array('AP', AP_r, 2*NT)
  call print_array('ARF', ARF_r, 2*NT)
  call end_test()

  ! Test 15: N=7, TRANSR='C', UPLO='U'
  ARF = (0.0d0, 0.0d0)
  call ztpttf('C', 'U', N, AP, ARF, INFO)
  call begin_test('n7_C_U')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_array('AP', AP_r, 2*NT)
  call print_array('ARF', ARF_r, 2*NT)
  call end_test()

  ! =============================================
  ! N=8 (even, larger) — all 4 combinations
  ! =============================================
  N = 8
  NT = N*(N+1)/2  ! = 36

  ! Fill AP with distinct complex values
  do i = 1, NT
    rval = dble(i) * 0.5d0
    ival = dble(i) * 0.2d0
    AP(i) = dcmplx(rval, ival)
  end do

  ! Test 16: N=8, TRANSR='N', UPLO='L'
  ARF = (0.0d0, 0.0d0)
  call ztpttf('N', 'L', N, AP, ARF, INFO)
  call begin_test('n8_N_L')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_array('AP', AP_r, 2*NT)
  call print_array('ARF', ARF_r, 2*NT)
  call end_test()

  ! Test 17: N=8, TRANSR='N', UPLO='U'
  ARF = (0.0d0, 0.0d0)
  call ztpttf('N', 'U', N, AP, ARF, INFO)
  call begin_test('n8_N_U')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_array('AP', AP_r, 2*NT)
  call print_array('ARF', ARF_r, 2*NT)
  call end_test()

  ! Test 18: N=8, TRANSR='C', UPLO='L'
  ARF = (0.0d0, 0.0d0)
  call ztpttf('C', 'L', N, AP, ARF, INFO)
  call begin_test('n8_C_L')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_array('AP', AP_r, 2*NT)
  call print_array('ARF', ARF_r, 2*NT)
  call end_test()

  ! Test 19: N=8, TRANSR='C', UPLO='U'
  ARF = (0.0d0, 0.0d0)
  call ztpttf('C', 'U', N, AP, ARF, INFO)
  call begin_test('n8_C_U')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_array('AP', AP_r, 2*NT)
  call print_array('ARF', ARF_r, 2*NT)
  call end_test()

end program
