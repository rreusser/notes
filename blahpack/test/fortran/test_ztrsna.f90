program test_ztrsna
  use test_utils
  implicit none

  integer, parameter :: MAXN = 4
  complex*16 :: T(MAXN, MAXN), VL(MAXN, MAXN), VR(MAXN, MAXN)
  complex*16 :: WORK(MAXN, MAXN+1), DUMMY(1)
  double precision :: T_r(2*MAXN*MAXN)
  double precision :: VL_r(2*MAXN*MAXN), VR_r(2*MAXN*MAXN)
  equivalence (T, T_r)
  equivalence (VL, VL_r)
  equivalence (VR, VR_r)
  logical :: SELEC(MAXN)
  double precision :: S(MAXN), SEP(MAXN), RWORK(MAXN)
  integer :: INFO, M, N, I, J, MM
  integer :: MOUT

  ! ---------- build a fixed T and compute its eigenvectors with ZTREVC ----------
  N = 4
  MM = MAXN

  call init_matrix(T, VL, VR)
  call ZTREVC('Both', 'All', SELEC, N, T, MAXN, VL, MAXN, VR, MAXN, MM, MOUT, WORK, RWORK, INFO)

  ! ============================================================
  ! Test 1: JOB='B' HOWMNY='A' (all eigenvalues and eigenvectors)
  ! ============================================================
  call init_matrix(T, VL, VR)
  call ZTREVC('Both', 'All', SELEC, N, T, MAXN, VL, MAXN, VR, MAXN, MM, MOUT, WORK, RWORK, INFO)
  do I = 1, MAXN
    S(I) = 0.0d0
    SEP(I) = 0.0d0
  end do
  do I = 1, MAXN
    do J = 1, MAXN+1
      WORK(I, J) = (0.0d0, 0.0d0)
    end do
  end do
  do I = 1, MAXN
    RWORK(I) = 0.0d0
  end do

  call ZTRSNA('Both', 'All', SELEC, N, T, MAXN, VL, MAXN, VR, MAXN, S, SEP, MM, M, WORK, MAXN, RWORK, INFO)
  call begin_test('job=B howmny=A')
  call print_array('T', T_r, 2*N*N)
  call print_array('VL', VL_r, 2*N*N)
  call print_array('VR', VR_r, 2*N*N)
  call print_array('S', S, N)
  call print_array('SEP', SEP, N)
  call print_int('M', M)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 2: JOB='E' HOWMNY='A' (eigenvalues only)
  ! ============================================================
  call init_matrix(T, VL, VR)
  call ZTREVC('Both', 'All', SELEC, N, T, MAXN, VL, MAXN, VR, MAXN, MM, MOUT, WORK, RWORK, INFO)
  do I = 1, MAXN
    S(I) = 0.0d0
    SEP(I) = 0.0d0
  end do
  call ZTRSNA('Eigcond', 'All', SELEC, N, T, MAXN, VL, MAXN, VR, MAXN, S, SEP, MM, M, WORK, MAXN, RWORK, INFO)
  call begin_test('job=E howmny=A')
  call print_array('S', S, N)
  call print_int('M', M)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 3: JOB='V' HOWMNY='A' (eigenvectors only)
  ! ============================================================
  call init_matrix(T, VL, VR)
  call ZTREVC('Both', 'All', SELEC, N, T, MAXN, VL, MAXN, VR, MAXN, MM, MOUT, WORK, RWORK, INFO)
  do I = 1, MAXN
    S(I) = 0.0d0
    SEP(I) = 0.0d0
  end do
  call ZTRSNA('Veccond', 'All', SELEC, N, T, MAXN, VL, MAXN, VR, MAXN, S, SEP, MM, M, WORK, MAXN, RWORK, INFO)
  call begin_test('job=V howmny=A')
  call print_array('SEP', SEP, N)
  call print_int('M', M)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 4: JOB='B' HOWMNY='S' select eigenvalues 1 and 3
  ! ============================================================
  call init_matrix(T, VL, VR)
  SELEC(1) = .TRUE.; SELEC(2) = .FALSE.; SELEC(3) = .TRUE.; SELEC(4) = .FALSE.
  call ZTREVC('Both', 'Selected', SELEC, N, T, MAXN, VL, MAXN, VR, MAXN, MM, MOUT, WORK, RWORK, INFO)
  do I = 1, MAXN
    S(I) = 0.0d0
    SEP(I) = 0.0d0
  end do
  SELEC(1) = .TRUE.; SELEC(2) = .FALSE.; SELEC(3) = .TRUE.; SELEC(4) = .FALSE.
  call ZTRSNA('Both', 'Selected', SELEC, N, T, MAXN, VL, MAXN, VR, MAXN, S, SEP, MM, M, WORK, MAXN, RWORK, INFO)
  call begin_test('job=B howmny=S sel 1,3')
  call print_array('S', S, 2)
  call print_array('SEP', SEP, 2)
  call print_int('M', M)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 5: N=1 quick return, JOB='B'
  ! ============================================================
  N = 1
  T(1,1) = (3.5d0, -1.25d0)
  VL(1,1) = (1.0d0, 0.0d0)
  VR(1,1) = (1.0d0, 0.0d0)
  SELEC(1) = .TRUE.
  S(1) = 0.0d0; SEP(1) = 0.0d0
  call ZTRSNA('Both', 'All', SELEC, N, T, MAXN, VL, MAXN, VR, MAXN, S, SEP, MM, M, WORK, MAXN, RWORK, INFO)
  call begin_test('N=1 job=B')
  call print_array('S', S, 1)
  call print_array('SEP', SEP, 1)
  call print_int('M', M)
  call print_int('info', INFO)
  call end_test()

contains

  subroutine init_matrix(T, VL, VR)
    complex*16, intent(out) :: T(MAXN, MAXN), VL(MAXN, MAXN), VR(MAXN, MAXN)
    integer :: i, j
    do j = 1, MAXN
      do i = 1, MAXN
        T(i,j) = (0.0d0, 0.0d0)
        VL(i,j) = (0.0d0, 0.0d0)
        VR(i,j) = (0.0d0, 0.0d0)
      end do
    end do
    T(1,1) = (1.0d0, 0.5d0); T(1,2) = (0.3d0, 0.1d0); T(1,3) = (0.2d0,-0.1d0); T(1,4) = (0.1d0, 0.05d0)
    T(2,2) = (2.0d0,-0.3d0); T(2,3) = (0.4d0, 0.2d0); T(2,4) = (0.15d0,-0.1d0)
    T(3,3) = (3.0d0, 1.0d0); T(3,4) = (0.5d0, 0.3d0)
    T(4,4) = (4.0d0,-0.5d0)
  end subroutine

end program
