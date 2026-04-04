program test_zlanhf
  use test_utils
  implicit none
  integer, parameter :: MAXN = 6
  integer, parameter :: MAXRFP = MAXN*(MAXN+1)/2
  complex*16 :: atr(MAXN, MAXN)
  complex*16 :: rfp(MAXRFP)
  double precision :: rfp_d(2*MAXRFP)
  equivalence (rfp, rfp_d)
  double precision :: work(MAXN)
  double precision :: result
  double precision :: zlanhf
  external :: zlanhf, ztrttf
  integer :: n, info, nrfp

  ! =====================================================
  ! Test with N=0
  ! =====================================================
  call begin_test('zlanhf_n0')
  result = zlanhf('M', 'N', 'U', 0, rfp, work)
  call print_scalar('result', result)
  call end_test()

  ! =====================================================
  ! Test with N=1
  ! =====================================================
  rfp(1) = (-3.5d0, 0.0d0)
  call begin_test('zlanhf_n1')
  result = zlanhf('M', 'N', 'U', 1, rfp, work)
  call print_scalar('result', result)
  nrfp = 1
  call print_array('rfp', rfp_d, 2*nrfp)
  call end_test()

  ! =====================================================
  ! N=5 (odd) Hermitian matrix
  ! =====================================================
  n = 5
  nrfp = n*(n+1)/2

  ! Build upper triangle in column-major 2D array
  atr = (0.0d0, 0.0d0)
  atr(1,1) = (2.0d0, 0.0d0)
  atr(1,2) = (3.0d0, 1.0d0)
  atr(2,2) = (5.0d0, 0.0d0)
  atr(1,3) = (-1.0d0, 2.0d0)
  atr(2,3) = (2.0d0, 0.5d0)
  atr(3,3) = (7.0d0, 0.0d0)
  atr(1,4) = (4.0d0, -3.0d0)
  atr(2,4) = (-6.0d0, 1.0d0)
  atr(3,4) = (1.0d0, 4.0d0)
  atr(4,4) = (8.0d0, 0.0d0)
  atr(1,5) = (0.5d0, 1.0d0)
  atr(2,5) = (1.0d0, 2.0d0)
  atr(3,5) = (3.0d0, -1.0d0)
  atr(4,5) = (-2.0d0, 0.5d0)
  atr(5,5) = (6.0d0, 0.0d0)

  ! --- TRANSR='N', UPLO='U' ---
  call ztrttf('N', 'U', n, atr, MAXN, rfp, info)

  call begin_test('zlanhf_5_NU_max')
  result = zlanhf('M', 'N', 'U', n, rfp, work)
  call print_scalar('result', result)
  call print_array('rfp', rfp_d, 2*nrfp)
  call end_test()

  call begin_test('zlanhf_5_NU_one')
  result = zlanhf('1', 'N', 'U', n, rfp, work)
  call print_scalar('result', result)
  call end_test()

  call begin_test('zlanhf_5_NU_inf')
  result = zlanhf('I', 'N', 'U', n, rfp, work)
  call print_scalar('result', result)
  call end_test()

  call begin_test('zlanhf_5_NU_frob')
  result = zlanhf('F', 'N', 'U', n, rfp, work)
  call print_scalar('result', result)
  call end_test()

  ! --- TRANSR='C', UPLO='U' ---
  call ztrttf('C', 'U', n, atr, MAXN, rfp, info)

  call begin_test('zlanhf_5_CU_max')
  result = zlanhf('M', 'C', 'U', n, rfp, work)
  call print_scalar('result', result)
  call print_array('rfp', rfp_d, 2*nrfp)
  call end_test()

  call begin_test('zlanhf_5_CU_one')
  result = zlanhf('1', 'C', 'U', n, rfp, work)
  call print_scalar('result', result)
  call end_test()

  call begin_test('zlanhf_5_CU_inf')
  result = zlanhf('I', 'C', 'U', n, rfp, work)
  call print_scalar('result', result)
  call end_test()

  call begin_test('zlanhf_5_CU_frob')
  result = zlanhf('F', 'C', 'U', n, rfp, work)
  call print_scalar('result', result)
  call end_test()

  ! Now build lower triangle
  atr = (0.0d0, 0.0d0)
  atr(1,1) = (2.0d0, 0.0d0)
  atr(2,1) = (3.0d0, -1.0d0)
  atr(2,2) = (5.0d0, 0.0d0)
  atr(3,1) = (-1.0d0, -2.0d0)
  atr(3,2) = (2.0d0, -0.5d0)
  atr(3,3) = (7.0d0, 0.0d0)
  atr(4,1) = (4.0d0, 3.0d0)
  atr(4,2) = (-6.0d0, -1.0d0)
  atr(4,3) = (1.0d0, -4.0d0)
  atr(4,4) = (8.0d0, 0.0d0)
  atr(5,1) = (0.5d0, -1.0d0)
  atr(5,2) = (1.0d0, -2.0d0)
  atr(5,3) = (3.0d0, 1.0d0)
  atr(5,4) = (-2.0d0, -0.5d0)
  atr(5,5) = (6.0d0, 0.0d0)

  ! --- TRANSR='N', UPLO='L' ---
  call ztrttf('N', 'L', n, atr, MAXN, rfp, info)

  call begin_test('zlanhf_5_NL_max')
  result = zlanhf('M', 'N', 'L', n, rfp, work)
  call print_scalar('result', result)
  call print_array('rfp', rfp_d, 2*nrfp)
  call end_test()

  call begin_test('zlanhf_5_NL_one')
  result = zlanhf('1', 'N', 'L', n, rfp, work)
  call print_scalar('result', result)
  call end_test()

  call begin_test('zlanhf_5_NL_inf')
  result = zlanhf('I', 'N', 'L', n, rfp, work)
  call print_scalar('result', result)
  call end_test()

  call begin_test('zlanhf_5_NL_frob')
  result = zlanhf('F', 'N', 'L', n, rfp, work)
  call print_scalar('result', result)
  call end_test()

  ! --- TRANSR='C', UPLO='L' ---
  call ztrttf('C', 'L', n, atr, MAXN, rfp, info)

  call begin_test('zlanhf_5_CL_max')
  result = zlanhf('M', 'C', 'L', n, rfp, work)
  call print_scalar('result', result)
  call print_array('rfp', rfp_d, 2*nrfp)
  call end_test()

  call begin_test('zlanhf_5_CL_one')
  result = zlanhf('1', 'C', 'L', n, rfp, work)
  call print_scalar('result', result)
  call end_test()

  call begin_test('zlanhf_5_CL_inf')
  result = zlanhf('I', 'C', 'L', n, rfp, work)
  call print_scalar('result', result)
  call end_test()

  call begin_test('zlanhf_5_CL_frob')
  result = zlanhf('F', 'C', 'L', n, rfp, work)
  call print_scalar('result', result)
  call end_test()

  ! =====================================================
  ! N=4 (even) Hermitian matrix
  ! =====================================================
  n = 4
  nrfp = n*(n+1)/2

  ! Upper triangle
  atr = (0.0d0, 0.0d0)
  atr(1,1) = (2.0d0, 0.0d0)
  atr(1,2) = (3.0d0, 1.0d0)
  atr(2,2) = (5.0d0, 0.0d0)
  atr(1,3) = (-1.0d0, 2.0d0)
  atr(2,3) = (2.0d0, 0.5d0)
  atr(3,3) = (7.0d0, 0.0d0)
  atr(1,4) = (4.0d0, -3.0d0)
  atr(2,4) = (-6.0d0, 1.0d0)
  atr(3,4) = (1.0d0, 4.0d0)
  atr(4,4) = (8.0d0, 0.0d0)

  ! --- TRANSR='N', UPLO='U' ---
  call ztrttf('N', 'U', n, atr, MAXN, rfp, info)

  call begin_test('zlanhf_4_NU_max')
  result = zlanhf('M', 'N', 'U', n, rfp, work)
  call print_scalar('result', result)
  call print_array('rfp', rfp_d, 2*nrfp)
  call end_test()

  call begin_test('zlanhf_4_NU_one')
  result = zlanhf('1', 'N', 'U', n, rfp, work)
  call print_scalar('result', result)
  call end_test()

  call begin_test('zlanhf_4_NU_inf')
  result = zlanhf('I', 'N', 'U', n, rfp, work)
  call print_scalar('result', result)
  call end_test()

  call begin_test('zlanhf_4_NU_frob')
  result = zlanhf('F', 'N', 'U', n, rfp, work)
  call print_scalar('result', result)
  call end_test()

  ! --- TRANSR='C', UPLO='U' ---
  call ztrttf('C', 'U', n, atr, MAXN, rfp, info)

  call begin_test('zlanhf_4_CU_max')
  result = zlanhf('M', 'C', 'U', n, rfp, work)
  call print_scalar('result', result)
  call print_array('rfp', rfp_d, 2*nrfp)
  call end_test()

  call begin_test('zlanhf_4_CU_one')
  result = zlanhf('1', 'C', 'U', n, rfp, work)
  call print_scalar('result', result)
  call end_test()

  call begin_test('zlanhf_4_CU_inf')
  result = zlanhf('I', 'C', 'U', n, rfp, work)
  call print_scalar('result', result)
  call end_test()

  call begin_test('zlanhf_4_CU_frob')
  result = zlanhf('F', 'C', 'U', n, rfp, work)
  call print_scalar('result', result)
  call end_test()

  ! Lower triangle
  atr = (0.0d0, 0.0d0)
  atr(1,1) = (2.0d0, 0.0d0)
  atr(2,1) = (3.0d0, -1.0d0)
  atr(2,2) = (5.0d0, 0.0d0)
  atr(3,1) = (-1.0d0, -2.0d0)
  atr(3,2) = (2.0d0, -0.5d0)
  atr(3,3) = (7.0d0, 0.0d0)
  atr(4,1) = (4.0d0, 3.0d0)
  atr(4,2) = (-6.0d0, -1.0d0)
  atr(4,3) = (1.0d0, -4.0d0)
  atr(4,4) = (8.0d0, 0.0d0)

  ! --- TRANSR='N', UPLO='L' ---
  call ztrttf('N', 'L', n, atr, MAXN, rfp, info)

  call begin_test('zlanhf_4_NL_max')
  result = zlanhf('M', 'N', 'L', n, rfp, work)
  call print_scalar('result', result)
  call print_array('rfp', rfp_d, 2*nrfp)
  call end_test()

  call begin_test('zlanhf_4_NL_one')
  result = zlanhf('1', 'N', 'L', n, rfp, work)
  call print_scalar('result', result)
  call end_test()

  call begin_test('zlanhf_4_NL_inf')
  result = zlanhf('I', 'N', 'L', n, rfp, work)
  call print_scalar('result', result)
  call end_test()

  call begin_test('zlanhf_4_NL_frob')
  result = zlanhf('F', 'N', 'L', n, rfp, work)
  call print_scalar('result', result)
  call end_test()

  ! --- TRANSR='C', UPLO='L' ---
  call ztrttf('C', 'L', n, atr, MAXN, rfp, info)

  call begin_test('zlanhf_4_CL_max')
  result = zlanhf('M', 'C', 'L', n, rfp, work)
  call print_scalar('result', result)
  call print_array('rfp', rfp_d, 2*nrfp)
  call end_test()

  call begin_test('zlanhf_4_CL_one')
  result = zlanhf('1', 'C', 'L', n, rfp, work)
  call print_scalar('result', result)
  call end_test()

  call begin_test('zlanhf_4_CL_inf')
  result = zlanhf('I', 'C', 'L', n, rfp, work)
  call print_scalar('result', result)
  call end_test()

  call begin_test('zlanhf_4_CL_frob')
  result = zlanhf('F', 'C', 'L', n, rfp, work)
  call print_scalar('result', result)
  call end_test()

  ! =====================================================
  ! N=3 (odd) with RFP data printed for JS test
  ! =====================================================
  n = 3
  nrfp = n*(n+1)/2
  atr = (0.0d0, 0.0d0)
  atr(1,1) = (1.0d0, 0.0d0)
  atr(1,2) = (2.0d0, 3.0d0)
  atr(2,2) = (4.0d0, 0.0d0)
  atr(1,3) = (5.0d0, -1.0d0)
  atr(2,3) = (0.0d0, 6.0d0)
  atr(3,3) = (9.0d0, 0.0d0)

  ! TRANSR='N', UPLO='U'
  call ztrttf('N', 'U', n, atr, MAXN, rfp, info)
  call begin_test('zlanhf_3_NU_max')
  result = zlanhf('M', 'N', 'U', n, rfp, work)
  call print_scalar('result', result)
  call print_array('rfp', rfp_d, 2*nrfp)
  call end_test()

  call begin_test('zlanhf_3_NU_one')
  result = zlanhf('1', 'N', 'U', n, rfp, work)
  call print_scalar('result', result)
  call end_test()

  call begin_test('zlanhf_3_NU_inf')
  result = zlanhf('I', 'N', 'U', n, rfp, work)
  call print_scalar('result', result)
  call end_test()

  call begin_test('zlanhf_3_NU_frob')
  result = zlanhf('F', 'N', 'U', n, rfp, work)
  call print_scalar('result', result)
  call end_test()

  ! TRANSR='C', UPLO='U'
  call ztrttf('C', 'U', n, atr, MAXN, rfp, info)
  call begin_test('zlanhf_3_CU_max')
  result = zlanhf('M', 'C', 'U', n, rfp, work)
  call print_scalar('result', result)
  call print_array('rfp', rfp_d, 2*nrfp)
  call end_test()

  call begin_test('zlanhf_3_CU_one')
  result = zlanhf('1', 'C', 'U', n, rfp, work)
  call print_scalar('result', result)
  call end_test()

  call begin_test('zlanhf_3_CU_inf')
  result = zlanhf('I', 'C', 'U', n, rfp, work)
  call print_scalar('result', result)
  call end_test()

  call begin_test('zlanhf_3_CU_frob')
  result = zlanhf('F', 'C', 'U', n, rfp, work)
  call print_scalar('result', result)
  call end_test()

  ! Lower triangle of same matrix
  atr = (0.0d0, 0.0d0)
  atr(1,1) = (1.0d0, 0.0d0)
  atr(2,1) = (2.0d0, -3.0d0)
  atr(2,2) = (4.0d0, 0.0d0)
  atr(3,1) = (5.0d0, 1.0d0)
  atr(3,2) = (0.0d0, -6.0d0)
  atr(3,3) = (9.0d0, 0.0d0)

  ! TRANSR='N', UPLO='L'
  call ztrttf('N', 'L', n, atr, MAXN, rfp, info)
  call begin_test('zlanhf_3_NL_max')
  result = zlanhf('M', 'N', 'L', n, rfp, work)
  call print_scalar('result', result)
  call print_array('rfp', rfp_d, 2*nrfp)
  call end_test()

  call begin_test('zlanhf_3_NL_one')
  result = zlanhf('1', 'N', 'L', n, rfp, work)
  call print_scalar('result', result)
  call end_test()

  call begin_test('zlanhf_3_NL_inf')
  result = zlanhf('I', 'N', 'L', n, rfp, work)
  call print_scalar('result', result)
  call end_test()

  call begin_test('zlanhf_3_NL_frob')
  result = zlanhf('F', 'N', 'L', n, rfp, work)
  call print_scalar('result', result)
  call end_test()

  ! TRANSR='C', UPLO='L'
  call ztrttf('C', 'L', n, atr, MAXN, rfp, info)
  call begin_test('zlanhf_3_CL_max')
  result = zlanhf('M', 'C', 'L', n, rfp, work)
  call print_scalar('result', result)
  call print_array('rfp', rfp_d, 2*nrfp)
  call end_test()

  call begin_test('zlanhf_3_CL_one')
  result = zlanhf('1', 'C', 'L', n, rfp, work)
  call print_scalar('result', result)
  call end_test()

  call begin_test('zlanhf_3_CL_inf')
  result = zlanhf('I', 'C', 'L', n, rfp, work)
  call print_scalar('result', result)
  call end_test()

  call begin_test('zlanhf_3_CL_frob')
  result = zlanhf('F', 'C', 'L', n, rfp, work)
  call print_scalar('result', result)
  call end_test()

  ! =====================================================
  ! N=2 (even) small matrix
  ! =====================================================
  n = 2
  nrfp = n*(n+1)/2

  atr = (0.0d0, 0.0d0)
  atr(1,1) = (3.0d0, 0.0d0)
  atr(1,2) = (1.0d0, 2.0d0)
  atr(2,2) = (5.0d0, 0.0d0)

  call ztrttf('N', 'U', n, atr, MAXN, rfp, info)
  call begin_test('zlanhf_2_NU_max')
  result = zlanhf('M', 'N', 'U', n, rfp, work)
  call print_scalar('result', result)
  call print_array('rfp', rfp_d, 2*nrfp)
  call end_test()

  call begin_test('zlanhf_2_NU_one')
  result = zlanhf('1', 'N', 'U', n, rfp, work)
  call print_scalar('result', result)
  call end_test()

  call begin_test('zlanhf_2_NU_frob')
  result = zlanhf('F', 'N', 'U', n, rfp, work)
  call print_scalar('result', result)
  call end_test()

  call ztrttf('C', 'U', n, atr, MAXN, rfp, info)
  call begin_test('zlanhf_2_CU_max')
  result = zlanhf('M', 'C', 'U', n, rfp, work)
  call print_scalar('result', result)
  call print_array('rfp', rfp_d, 2*nrfp)
  call end_test()

  call begin_test('zlanhf_2_CU_one')
  result = zlanhf('1', 'C', 'U', n, rfp, work)
  call print_scalar('result', result)
  call end_test()

  call begin_test('zlanhf_2_CU_frob')
  result = zlanhf('F', 'C', 'U', n, rfp, work)
  call print_scalar('result', result)
  call end_test()

  atr = (0.0d0, 0.0d0)
  atr(1,1) = (3.0d0, 0.0d0)
  atr(2,1) = (1.0d0, -2.0d0)
  atr(2,2) = (5.0d0, 0.0d0)

  call ztrttf('N', 'L', n, atr, MAXN, rfp, info)
  call begin_test('zlanhf_2_NL_max')
  result = zlanhf('M', 'N', 'L', n, rfp, work)
  call print_scalar('result', result)
  call print_array('rfp', rfp_d, 2*nrfp)
  call end_test()

  call begin_test('zlanhf_2_NL_one')
  result = zlanhf('1', 'N', 'L', n, rfp, work)
  call print_scalar('result', result)
  call end_test()

  call begin_test('zlanhf_2_NL_frob')
  result = zlanhf('F', 'N', 'L', n, rfp, work)
  call print_scalar('result', result)
  call end_test()

  call ztrttf('C', 'L', n, atr, MAXN, rfp, info)
  call begin_test('zlanhf_2_CL_max')
  result = zlanhf('M', 'C', 'L', n, rfp, work)
  call print_scalar('result', result)
  call print_array('rfp', rfp_d, 2*nrfp)
  call end_test()

  call begin_test('zlanhf_2_CL_one')
  result = zlanhf('1', 'C', 'L', n, rfp, work)
  call print_scalar('result', result)
  call end_test()

  call begin_test('zlanhf_2_CL_frob')
  result = zlanhf('F', 'C', 'L', n, rfp, work)
  call print_scalar('result', result)
  call end_test()

end program
