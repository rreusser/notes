program test_zgeev
  use test_utils
  implicit none

  integer, parameter :: NMAX = 4
  complex*16 :: A(NMAX,NMAX), W(NMAX), VL(NMAX,NMAX), VR(NMAX,NMAX)
  complex*16 :: WORK(200)
  double precision :: RWORK(200)
  double precision :: A_r(2*NMAX*NMAX), W_r(2*NMAX), VL_r(2*NMAX*NMAX), VR_r(2*NMAX*NMAX)
  equivalence (A, A_r)
  equivalence (W, W_r)
  equivalence (VL, VL_r)
  equivalence (VR, VR_r)
  integer :: INFO, N, LWORK

  LWORK = 200

  ! Test 1: N=0 quick return
  N = 0
  INFO = -1
  call ZGEEV('N', 'N', N, A, NMAX, W, VL, NMAX, VR, NMAX, WORK, LWORK, RWORK, INFO)
  call begin_test('n0')
  call print_int('info', INFO)
  call end_test()

  ! Test 2: N=1, eigenvalues only
  N = 1
  A(1,1) = dcmplx(3.0d0, 2.0d0)
  call ZGEEV('N', 'N', N, A, NMAX, W, VL, NMAX, VR, NMAX, WORK, LWORK, RWORK, INFO)
  call begin_test('n1_eigvals_only')
  call print_int('info', INFO)
  call print_array('w', W_r, 2)
  call end_test()

  ! Test 3: N=1, with right eigenvector
  A(1,1) = dcmplx(5.0d0, -1.0d0)
  call ZGEEV('N', 'V', N, A, NMAX, W, VL, NMAX, VR, NMAX, WORK, LWORK, RWORK, INFO)
  call begin_test('n1_right')
  call print_int('info', INFO)
  call print_array('w', W_r, 2)
  call print_array('VR', VR_r, 2)
  call end_test()

  ! Test 4: N=2 diagonal matrix, right eigenvectors
  N = 2
  A(1,1) = dcmplx(1.0d0, 0.0d0)
  A(1,2) = dcmplx(0.0d0, 0.0d0)
  A(2,1) = dcmplx(0.0d0, 0.0d0)
  A(2,2) = dcmplx(2.0d0, 0.0d0)
  call ZGEEV('N', 'V', N, A, NMAX, W, VL, NMAX, VR, NMAX, WORK, LWORK, RWORK, INFO)
  call begin_test('n2_diagonal_right')
  call print_int('info', INFO)
  call print_array('w', W_r, 2*N)
  call print_array('VR', VR_r, 2*N*N)
  call end_test()

  ! Test 5: N=2 general complex, both eigenvectors
  N = 2
  A(1,1) = dcmplx(1.0d0, 2.0d0)
  A(1,2) = dcmplx(3.0d0, 0.0d0)
  A(2,1) = dcmplx(0.0d0, 1.0d0)
  A(2,2) = dcmplx(4.0d0, -1.0d0)
  call ZGEEV('V', 'V', N, A, NMAX, W, VL, NMAX, VR, NMAX, WORK, LWORK, RWORK, INFO)
  call begin_test('n2_general_both')
  call print_int('info', INFO)
  call print_array('w', W_r, 2*N)
  call print_array('VL', VL_r, 2*N*N)
  call print_array('VR', VR_r, 2*N*N)
  call end_test()

  ! Test 6: N=3 general complex, right eigenvectors only
  N = 3
  A(1,1) = dcmplx(1.0d0, 0.0d0)
  A(1,2) = dcmplx(2.0d0, 1.0d0)
  A(1,3) = dcmplx(0.0d0, 0.0d0)
  A(2,1) = dcmplx(0.0d0, -1.0d0)
  A(2,2) = dcmplx(3.0d0, 0.0d0)
  A(2,3) = dcmplx(1.0d0, 0.5d0)
  A(3,1) = dcmplx(0.0d0, 0.0d0)
  A(3,2) = dcmplx(0.0d0, 0.0d0)
  A(3,3) = dcmplx(5.0d0, -2.0d0)
  call ZGEEV('N', 'V', N, A, NMAX, W, VL, NMAX, VR, NMAX, WORK, LWORK, RWORK, INFO)
  call begin_test('n3_right')
  call print_int('info', INFO)
  call print_array('w', W_r, 2*N)
  call print_array('VR', VR_r, 2*N*N)
  call end_test()

  ! Test 7: N=4 diagonally dominant, both eigenvectors
  N = 4
  A(1,1) = dcmplx(10.0d0, 0.0d0)
  A(1,2) = dcmplx(1.0d0, 0.5d0)
  A(1,3) = dcmplx(0.0d0, 0.0d0)
  A(1,4) = dcmplx(0.0d0, 0.0d0)
  A(2,1) = dcmplx(0.5d0, -0.5d0)
  A(2,2) = dcmplx(20.0d0, 0.0d0)
  A(2,3) = dcmplx(1.0d0, 0.0d0)
  A(2,4) = dcmplx(0.0d0, 0.0d0)
  A(3,1) = dcmplx(0.0d0, 0.0d0)
  A(3,2) = dcmplx(0.0d0, 1.0d0)
  A(3,3) = dcmplx(30.0d0, 0.0d0)
  A(3,4) = dcmplx(0.5d0, 0.5d0)
  A(4,1) = dcmplx(0.0d0, 0.0d0)
  A(4,2) = dcmplx(0.0d0, 0.0d0)
  A(4,3) = dcmplx(0.5d0, -0.5d0)
  A(4,4) = dcmplx(40.0d0, 0.0d0)
  call ZGEEV('V', 'V', N, A, NMAX, W, VL, NMAX, VR, NMAX, WORK, LWORK, RWORK, INFO)
  call begin_test('n4_diagdom_both')
  call print_int('info', INFO)
  call print_array('w', W_r, 2*N)
  call print_array('VL', VL_r, 2*N*N)
  call print_array('VR', VR_r, 2*N*N)
  call end_test()

  ! Test 8: N=2 left eigenvectors only
  N = 2
  A(1,1) = dcmplx(2.0d0, 0.0d0)
  A(1,2) = dcmplx(1.0d0, 1.0d0)
  A(2,1) = dcmplx(0.0d0, 0.0d0)
  A(2,2) = dcmplx(3.0d0, 0.0d0)
  call ZGEEV('V', 'N', N, A, NMAX, W, VL, NMAX, VR, NMAX, WORK, LWORK, RWORK, INFO)
  call begin_test('n2_left_only')
  call print_int('info', INFO)
  call print_array('w', W_r, 2*N)
  call print_array('VL', VL_r, 2*N*N)
  call end_test()

end program
