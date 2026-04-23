program test_zgees
  use test_utils
  implicit none

  integer, parameter :: NMAX = 6
  integer :: N, SDIM, INFO, LDA, LDVS, LWORK, I, J
  complex*16 :: A(NMAX,NMAX), VS(NMAX,NMAX), W(NMAX), WORK(10*NMAX)
  double precision :: RWORK(NMAX)
  logical :: BWORK(NMAX)
  double precision :: A_r(2*NMAX*NMAX), VS_r(2*NMAX*NMAX), W_r(2*NMAX)
  equivalence (A, A_r)
  equivalence (VS, VS_r)
  equivalence (W, W_r)

  ! ----------------------------
  ! Test 1: N=0 (quick return)
  ! ----------------------------
  N = 0
  call ZGEES('V', 'N', select_none, N, A, NMAX, SDIM, W, VS, NMAX, &
             WORK, 10*NMAX, RWORK, BWORK, INFO)
  call begin_test('n0')
  call print_int('info', INFO)
  call print_int('sdim', SDIM)
  call end_test()

  ! ----------------------------
  ! Test 2: N=1
  ! ----------------------------
  N = 1
  A(1,1) = (3.0d0, 2.0d0)
  call ZGEES('V', 'N', select_none, N, A, NMAX, SDIM, W, VS, NMAX, &
             WORK, 10*NMAX, RWORK, BWORK, INFO)
  call begin_test('n1')
  call print_int('info', INFO)
  call print_int('sdim', SDIM)
  call print_array('w', W_r, 2*N)
  call print_array('vs', VS_r, 2*N*N)
  call end_test()

  ! ----------------------------
  ! Test 3: N=3, JOBVS='V', SORT='N'
  ! ----------------------------
  N = 3
  A(1,1) = (1.0d0, 0.0d0)
  A(1,2) = (2.0d0, 1.0d0)
  A(1,3) = (3.0d0, -1.0d0)
  A(2,1) = (0.0d0, 1.0d0)
  A(2,2) = (4.0d0, 0.0d0)
  A(2,3) = (5.0d0, 2.0d0)
  A(3,1) = (0.0d0, 0.0d0)
  A(3,2) = (0.0d0, -1.0d0)
  A(3,3) = (6.0d0, 0.0d0)
  ! This is already upper triangular, so eigenvalues are diagonal elements
  call ZGEES('V', 'N', select_none, N, A, NMAX, SDIM, W, VS, NMAX, &
             WORK, 10*NMAX, RWORK, BWORK, INFO)
  call begin_test('n3_nosort')
  call print_int('info', INFO)
  call print_int('sdim', SDIM)
  call print_array('w', W_r, 2*N)
  call end_test()

  ! ----------------------------
  ! Test 4: N=3, JOBVS='V', SORT='S'
  ! Select eigenvalues with real part > 3
  ! ----------------------------
  N = 3
  A(1,1) = (1.0d0, 0.0d0)
  A(1,2) = (2.0d0, 1.0d0)
  A(1,3) = (3.0d0, -1.0d0)
  A(2,1) = (0.0d0, 1.0d0)
  A(2,2) = (4.0d0, 0.0d0)
  A(2,3) = (5.0d0, 2.0d0)
  A(3,1) = (0.0d0, 0.0d0)
  A(3,2) = (0.0d0, -1.0d0)
  A(3,3) = (6.0d0, 0.0d0)
  call ZGEES('V', 'S', select_gt3, N, A, NMAX, SDIM, W, VS, NMAX, &
             WORK, 10*NMAX, RWORK, BWORK, INFO)
  call begin_test('n3_sort')
  call print_int('info', INFO)
  call print_int('sdim', SDIM)
  call print_array('w', W_r, 2*N)
  call end_test()

  ! ----------------------------
  ! Test 5: N=4, JOBVS='N', general matrix
  ! ----------------------------
  N = 4
  A(1,1) = (2.0d0, 1.0d0)
  A(1,2) = (1.0d0, 0.5d0)
  A(1,3) = (0.0d0, 0.0d0)
  A(1,4) = (0.0d0, 0.0d0)
  A(2,1) = (1.0d0, -0.5d0)
  A(2,2) = (3.0d0, 0.0d0)
  A(2,3) = (1.0d0, 1.0d0)
  A(2,4) = (0.0d0, 0.0d0)
  A(3,1) = (0.0d0, 0.0d0)
  A(3,2) = (1.0d0, -1.0d0)
  A(3,3) = (4.0d0, 0.5d0)
  A(3,4) = (2.0d0, 0.0d0)
  A(4,1) = (0.0d0, 0.0d0)
  A(4,2) = (0.0d0, 0.0d0)
  A(4,3) = (1.0d0, 0.0d0)
  A(4,4) = (5.0d0, -1.0d0)
  call ZGEES('N', 'N', select_none, N, A, NMAX, SDIM, W, VS, NMAX, &
             WORK, 10*NMAX, RWORK, BWORK, INFO)
  call begin_test('n4_noschur')
  call print_int('info', INFO)
  call print_int('sdim', SDIM)
  call print_array('w', W_r, 2*N)
  call end_test()

  ! ----------------------------
  ! Test 6: N=4, JOBVS='V', general dense matrix
  ! ----------------------------
  N = 4
  A(1,1) = (2.0d0, 1.0d0)
  A(1,2) = (1.0d0, 0.5d0)
  A(1,3) = (0.0d0, 0.0d0)
  A(1,4) = (0.0d0, 0.0d0)
  A(2,1) = (1.0d0, -0.5d0)
  A(2,2) = (3.0d0, 0.0d0)
  A(2,3) = (1.0d0, 1.0d0)
  A(2,4) = (0.0d0, 0.0d0)
  A(3,1) = (0.0d0, 0.0d0)
  A(3,2) = (1.0d0, -1.0d0)
  A(3,3) = (4.0d0, 0.5d0)
  A(3,4) = (2.0d0, 0.0d0)
  A(4,1) = (0.0d0, 0.0d0)
  A(4,2) = (0.0d0, 0.0d0)
  A(4,3) = (1.0d0, 0.0d0)
  A(4,4) = (5.0d0, -1.0d0)
  call ZGEES('V', 'N', select_none, N, A, NMAX, SDIM, W, VS, NMAX, &
             WORK, 10*NMAX, RWORK, BWORK, INFO)
  call begin_test('n4_schur')
  call print_int('info', INFO)
  call print_int('sdim', SDIM)
  call print_array('w', W_r, 2*N)
  call print_array('T', A_r, 2*N*N)
  call print_array('vs', VS_r, 2*N*N)
  call end_test()

contains

  logical function select_none(w)
    complex*16 :: w
    select_none = .false.
  end function

  logical function select_gt3(w)
    complex*16 :: w
    select_gt3 = (dble(w) > 3.0d0)
  end function

end program test_zgees
