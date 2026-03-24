program test_dtrevc3
  use test_utils
  implicit none

  integer, parameter :: MAXN = 4
  double precision :: T(MAXN, MAXN), VL(MAXN, MAXN), VR(MAXN, MAXN)
  double precision :: WORK(3*MAXN)
  logical :: SELEC(MAXN)
  integer :: INFO, M, N, I

  ! ============================================================
  ! Test 1: SIDE='R', HOWMNY='A', 4x4 with 2 real + 1 complex pair
  ! Upper quasi-triangular:
  !   T = [ 1   0.5  0.2  0.1 ]
  !       [ 0   2    0.3  0.15]
  !       [ 0   0    3   -0.5 ]
  !       [ 0   0    0.8  3   ]
  ! Eigenvalues: 1, 2, 3+/-sqrt(0.4)i (complex pair from 2x2 block)
  ! ============================================================
  N = 4
  T = 0.0d0; VR = 0.0d0; VL = 0.0d0; WORK = 0.0d0
  T(1,1) = 1.0d0; T(1,2) = 0.5d0; T(1,3) = 0.2d0; T(1,4) = 0.1d0
  T(2,2) = 2.0d0; T(2,3) = 0.3d0; T(2,4) = 0.15d0
  T(3,3) = 3.0d0; T(3,4) = -0.5d0
  T(4,3) = 0.8d0; T(4,4) = 3.0d0
  call DTREVC3('R', 'A', SELEC, N, T, MAXN, VL, MAXN, VR, MAXN, N, M, WORK, 3*MAXN, INFO)
  call begin_test('right all 4x4')
  call print_matrix('VR', VR, MAXN, N, N)
  call print_int('M', M)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 2: SIDE='L', HOWMNY='A', same matrix
  ! ============================================================
  T = 0.0d0; VR = 0.0d0; VL = 0.0d0; WORK = 0.0d0
  T(1,1) = 1.0d0; T(1,2) = 0.5d0; T(1,3) = 0.2d0; T(1,4) = 0.1d0
  T(2,2) = 2.0d0; T(2,3) = 0.3d0; T(2,4) = 0.15d0
  T(3,3) = 3.0d0; T(3,4) = -0.5d0
  T(4,3) = 0.8d0; T(4,4) = 3.0d0
  call DTREVC3('L', 'A', SELEC, N, T, MAXN, VL, MAXN, VR, MAXN, N, M, WORK, 3*MAXN, INFO)
  call begin_test('left all 4x4')
  call print_matrix('VL', VL, MAXN, N, N)
  call print_int('M', M)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 3: SIDE='B', HOWMNY='A', same matrix
  ! ============================================================
  T = 0.0d0; VR = 0.0d0; VL = 0.0d0; WORK = 0.0d0
  T(1,1) = 1.0d0; T(1,2) = 0.5d0; T(1,3) = 0.2d0; T(1,4) = 0.1d0
  T(2,2) = 2.0d0; T(2,3) = 0.3d0; T(2,4) = 0.15d0
  T(3,3) = 3.0d0; T(3,4) = -0.5d0
  T(4,3) = 0.8d0; T(4,4) = 3.0d0
  call DTREVC3('B', 'A', SELEC, N, T, MAXN, VL, MAXN, VR, MAXN, N, M, WORK, 3*MAXN, INFO)
  call begin_test('both all 4x4')
  call print_matrix('VR', VR, MAXN, N, N)
  call print_matrix('VL', VL, MAXN, N, N)
  call print_int('M', M)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 4: SIDE='R', HOWMNY='S', select only eigenvalue 1 and the complex pair
  ! ============================================================
  T = 0.0d0; VR = 0.0d0; VL = 0.0d0; WORK = 0.0d0
  T(1,1) = 1.0d0; T(1,2) = 0.5d0; T(1,3) = 0.2d0; T(1,4) = 0.1d0
  T(2,2) = 2.0d0; T(2,3) = 0.3d0; T(2,4) = 0.15d0
  T(3,3) = 3.0d0; T(3,4) = -0.5d0
  T(4,3) = 0.8d0; T(4,4) = 3.0d0
  SELEC(1) = .TRUE.
  SELEC(2) = .FALSE.
  SELEC(3) = .TRUE.   ! selects the complex pair (both 3 and 4)
  SELEC(4) = .TRUE.
  call DTREVC3('R', 'S', SELEC, N, T, MAXN, VL, MAXN, VR, MAXN, N, M, WORK, 3*MAXN, INFO)
  call begin_test('right selected 4x4')
  call print_matrix('VR', VR, MAXN, N, M)
  call print_int('M', M)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 5: SIDE='R', HOWMNY='B' (backtransform), 4x4 with identity as Q
  ! ============================================================
  T = 0.0d0; VR = 0.0d0; VL = 0.0d0; WORK = 0.0d0
  T(1,1) = 1.0d0; T(1,2) = 0.5d0; T(1,3) = 0.2d0; T(1,4) = 0.1d0
  T(2,2) = 2.0d0; T(2,3) = 0.3d0; T(2,4) = 0.15d0
  T(3,3) = 3.0d0; T(3,4) = -0.5d0
  T(4,3) = 0.8d0; T(4,4) = 3.0d0
  ! VR starts as identity (Q)
  do I = 1, N
    VR(I, I) = 1.0d0
  end do
  call DTREVC3('R', 'B', SELEC, N, T, MAXN, VL, MAXN, VR, MAXN, N, M, WORK, 3*MAXN, INFO)
  call begin_test('right backtransform 4x4')
  call print_matrix('VR', VR, MAXN, N, N)
  call print_int('M', M)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 6: All real eigenvalues (purely upper triangular, no 2x2 blocks)
  ! T = diag(5, 3, 1, -1) with some upper triangular entries
  ! ============================================================
  N = 4
  T = 0.0d0; VR = 0.0d0; VL = 0.0d0; WORK = 0.0d0
  T(1,1) = 5.0d0; T(1,2) = 1.0d0; T(1,3) = 0.5d0; T(1,4) = 0.2d0
  T(2,2) = 3.0d0; T(2,3) = 0.8d0; T(2,4) = 0.3d0
  T(3,3) = 1.0d0; T(3,4) = 0.6d0
  T(4,4) = -1.0d0
  call DTREVC3('R', 'A', SELEC, N, T, MAXN, VL, MAXN, VR, MAXN, N, M, WORK, 3*MAXN, INFO)
  call begin_test('right all real 4x4')
  call print_matrix('VR', VR, MAXN, N, N)
  call print_int('M', M)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 7: N=1 trivial
  ! ============================================================
  N = 1
  T = 0.0d0; VR = 0.0d0; VL = 0.0d0; WORK = 0.0d0
  T(1,1) = 7.0d0
  call DTREVC3('R', 'A', SELEC, N, T, MAXN, VL, MAXN, VR, MAXN, 1, M, WORK, 3, INFO)
  call begin_test('right N=1')
  call print_matrix('VR', VR, MAXN, 1, 1)
  call print_int('M', M)
  call print_int('info', INFO)
  call end_test()

end program
