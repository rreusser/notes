program test_ztfttp
  use test_utils
  implicit none
  integer, parameter :: NMAX = 8
  integer :: INFO, i, N, NT
  complex*16 :: ARF(NMAX*(NMAX+1)/2), AP(NMAX*(NMAX+1)/2)
  double precision :: ARF_r(NMAX*(NMAX+1))
  double precision :: AP_r(NMAX*(NMAX+1))
  equivalence (ARF, ARF_r)
  equivalence (AP, AP_r)

  ! =============================================
  ! Edge cases
  ! =============================================

  ! Test 1: N=0 quick return
  AP = (0.0d0, 0.0d0)
  call ztfttp('N', 'L', 0, ARF, AP, INFO)
  call begin_test('n0')
  call print_int('info', INFO)
  call end_test()

  ! Test 2: N=1, normal
  ARF(1) = (42.0d0, 7.0d0)
  AP = (0.0d0, 0.0d0)
  call ztfttp('N', 'L', 1, ARF, AP, INFO)
  call begin_test('n1_N')
  call print_int('info', INFO)
  call print_array('AP', AP_r, 2)
  call end_test()

  ! Test 3: N=1, conjugate-transpose
  ARF(1) = (99.0d0, -3.0d0)
  AP = (0.0d0, 0.0d0)
  call ztfttp('C', 'U', 1, ARF, AP, INFO)
  call begin_test('n1_C')
  call print_int('info', INFO)
  call print_array('AP', AP_r, 2)
  call end_test()

  ! =============================================
  ! N=5 (odd) — all 4 combinations
  ! =============================================
  N = 5
  NT = N*(N+1)/2  ! = 15

  ! Fill ARF with distinct complex values
  do i = 1, NT
    ARF(i) = dcmplx(dble(i), dble(i)*0.1d0)
  end do

  ! Test 4: N=5, TRANSR='N', UPLO='L' (odd, normal, lower)
  AP = (0.0d0, 0.0d0)
  call ztfttp('N', 'L', N, ARF, AP, INFO)
  call begin_test('n5_N_L')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_array('ARF', ARF_r, 2*NT)
  call print_array('AP', AP_r, 2*NT)
  call end_test()

  ! Test 5: N=5, TRANSR='N', UPLO='U' (odd, normal, upper)
  AP = (0.0d0, 0.0d0)
  call ztfttp('N', 'U', N, ARF, AP, INFO)
  call begin_test('n5_N_U')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_array('ARF', ARF_r, 2*NT)
  call print_array('AP', AP_r, 2*NT)
  call end_test()

  ! Test 6: N=5, TRANSR='C', UPLO='L' (odd, conjugate-transpose, lower)
  AP = (0.0d0, 0.0d0)
  call ztfttp('C', 'L', N, ARF, AP, INFO)
  call begin_test('n5_C_L')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_array('ARF', ARF_r, 2*NT)
  call print_array('AP', AP_r, 2*NT)
  call end_test()

  ! Test 7: N=5, TRANSR='C', UPLO='U' (odd, conjugate-transpose, upper)
  AP = (0.0d0, 0.0d0)
  call ztfttp('C', 'U', N, ARF, AP, INFO)
  call begin_test('n5_C_U')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_array('ARF', ARF_r, 2*NT)
  call print_array('AP', AP_r, 2*NT)
  call end_test()

  ! =============================================
  ! N=6 (even) — all 4 combinations
  ! =============================================
  N = 6
  NT = N*(N+1)/2  ! = 21

  ! Fill ARF with distinct complex values
  do i = 1, NT
    ARF(i) = dcmplx(dble(i), dble(i)*0.5d0)
  end do

  ! Test 8: N=6, TRANSR='N', UPLO='L' (even, normal, lower)
  AP = (0.0d0, 0.0d0)
  call ztfttp('N', 'L', N, ARF, AP, INFO)
  call begin_test('n6_N_L')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_array('ARF', ARF_r, 2*NT)
  call print_array('AP', AP_r, 2*NT)
  call end_test()

  ! Test 9: N=6, TRANSR='N', UPLO='U' (even, normal, upper)
  AP = (0.0d0, 0.0d0)
  call ztfttp('N', 'U', N, ARF, AP, INFO)
  call begin_test('n6_N_U')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_array('ARF', ARF_r, 2*NT)
  call print_array('AP', AP_r, 2*NT)
  call end_test()

  ! Test 10: N=6, TRANSR='C', UPLO='L' (even, conjugate-transpose, lower)
  AP = (0.0d0, 0.0d0)
  call ztfttp('C', 'L', N, ARF, AP, INFO)
  call begin_test('n6_C_L')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_array('ARF', ARF_r, 2*NT)
  call print_array('AP', AP_r, 2*NT)
  call end_test()

  ! Test 11: N=6, TRANSR='C', UPLO='U' (even, conjugate-transpose, upper)
  AP = (0.0d0, 0.0d0)
  call ztfttp('C', 'U', N, ARF, AP, INFO)
  call begin_test('n6_C_U')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_array('ARF', ARF_r, 2*NT)
  call print_array('AP', AP_r, 2*NT)
  call end_test()

  ! =============================================
  ! N=7 (odd, larger) — all 4 combinations
  ! =============================================
  N = 7
  NT = N*(N+1)/2  ! = 28

  ! Fill ARF with distinct complex values
  do i = 1, NT
    ARF(i) = dcmplx(dble(i)*0.1d0, dble(i)*0.2d0)
  end do

  ! Test 12: N=7, TRANSR='N', UPLO='L' (odd, normal, lower)
  AP = (0.0d0, 0.0d0)
  call ztfttp('N', 'L', N, ARF, AP, INFO)
  call begin_test('n7_N_L')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_array('ARF', ARF_r, 2*NT)
  call print_array('AP', AP_r, 2*NT)
  call end_test()

  ! Test 13: N=7, TRANSR='N', UPLO='U' (odd, normal, upper)
  AP = (0.0d0, 0.0d0)
  call ztfttp('N', 'U', N, ARF, AP, INFO)
  call begin_test('n7_N_U')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_array('ARF', ARF_r, 2*NT)
  call print_array('AP', AP_r, 2*NT)
  call end_test()

  ! Test 14: N=7, TRANSR='C', UPLO='L' (odd, conjugate-transpose, lower)
  AP = (0.0d0, 0.0d0)
  call ztfttp('C', 'L', N, ARF, AP, INFO)
  call begin_test('n7_C_L')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_array('ARF', ARF_r, 2*NT)
  call print_array('AP', AP_r, 2*NT)
  call end_test()

  ! Test 15: N=7, TRANSR='C', UPLO='U' (odd, conjugate-transpose, upper)
  AP = (0.0d0, 0.0d0)
  call ztfttp('C', 'U', N, ARF, AP, INFO)
  call begin_test('n7_C_U')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_array('ARF', ARF_r, 2*NT)
  call print_array('AP', AP_r, 2*NT)
  call end_test()

  ! =============================================
  ! N=8 (even, larger) — all 4 combinations
  ! =============================================
  N = 8
  NT = N*(N+1)/2  ! = 36

  ! Fill ARF with distinct complex values
  do i = 1, NT
    ARF(i) = dcmplx(dble(i)*0.5d0, dble(i)*0.3d0)
  end do

  ! Test 16: N=8, TRANSR='N', UPLO='L' (even, normal, lower)
  AP = (0.0d0, 0.0d0)
  call ztfttp('N', 'L', N, ARF, AP, INFO)
  call begin_test('n8_N_L')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_array('ARF', ARF_r, 2*NT)
  call print_array('AP', AP_r, 2*NT)
  call end_test()

  ! Test 17: N=8, TRANSR='N', UPLO='U' (even, normal, upper)
  AP = (0.0d0, 0.0d0)
  call ztfttp('N', 'U', N, ARF, AP, INFO)
  call begin_test('n8_N_U')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_array('ARF', ARF_r, 2*NT)
  call print_array('AP', AP_r, 2*NT)
  call end_test()

  ! Test 18: N=8, TRANSR='C', UPLO='L' (even, conjugate-transpose, lower)
  AP = (0.0d0, 0.0d0)
  call ztfttp('C', 'L', N, ARF, AP, INFO)
  call begin_test('n8_C_L')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_array('ARF', ARF_r, 2*NT)
  call print_array('AP', AP_r, 2*NT)
  call end_test()

  ! Test 19: N=8, TRANSR='C', UPLO='U' (even, conjugate-transpose, upper)
  AP = (0.0d0, 0.0d0)
  call ztfttp('C', 'U', N, ARF, AP, INFO)
  call begin_test('n8_C_U')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_array('ARF', ARF_r, 2*NT)
  call print_array('AP', AP_r, 2*NT)
  call end_test()

end program
