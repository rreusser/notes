program test_zunghr
  use test_utils
  implicit none

  integer, parameter :: MAXN = 10, LWORK = 2048
  double precision :: a_r(2*MAXN*MAXN), tau_r(2*MAXN), work_r(2*LWORK)
  double precision :: qtq_r(2*MAXN*MAXN)
  complex*16 :: A(MAXN*MAXN), TAU(MAXN), WORK(LWORK)
  complex*16 :: QtQ(MAXN*MAXN)
  equivalence (A, a_r)
  equivalence (TAU, tau_r)
  equivalence (WORK, work_r)
  equivalence (QtQ, qtq_r)
  integer :: INFO, i, j, k, N, LDA, ILO, IHI

  ! ---------------------------------------------------------------
  ! Test 1: 5x5 full range (ILO=1, IHI=5)
  ! ---------------------------------------------------------------
  N = 5; LDA = 5; ILO = 1; IHI = 5
  A = (0.0d0, 0.0d0)
  A(1)  = (2.0d0, 1.0d0);  A(6)  = (1.0d0, -0.5d0); A(11) = (3.0d0, 0.0d0);  A(16) = (1.0d0, 1.0d0);  A(21) = (4.0d0, -1.0d0)
  A(2)  = (1.0d0, 0.5d0);  A(7)  = (4.0d0, 0.0d0);  A(12) = (1.0d0, -1.0d0); A(17) = (2.0d0, 0.0d0);  A(22) = (1.0d0, 0.5d0)
  A(3)  = (3.0d0, -1.0d0); A(8)  = (1.0d0, 1.0d0);  A(13) = (5.0d0, 0.5d0);  A(18) = (1.0d0, -0.5d0); A(23) = (2.0d0, 0.0d0)
  A(4)  = (1.0d0, 0.0d0);  A(9)  = (2.0d0, -0.5d0); A(14) = (1.0d0, 0.0d0);  A(19) = (6.0d0, 1.0d0);  A(24) = (1.0d0, -1.0d0)
  A(5)  = (4.0d0, 1.0d0);  A(10) = (1.0d0, 0.0d0);  A(15) = (2.0d0, 1.0d0);  A(20) = (1.0d0, 0.5d0);  A(25) = (7.0d0, 0.0d0)
  TAU = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0)
  call ZGEHRD(N, ILO, IHI, A, LDA, TAU, WORK, LWORK, INFO)
  call ZUNGHR(N, ILO, IHI, A, LDA, TAU, WORK, LWORK, INFO)
  ! Verify Q^H * Q = I: QtQ(i,j) = sum_k conjg(A(k,i)) * A(k,j)
  QtQ = (0.0d0, 0.0d0)
  do i = 1, N
    do j = 1, N
      do k = 1, N
        QtQ((j-1)*N + i) = QtQ((j-1)*N + i) + conjg(A((i-1)*N + k)) * A((j-1)*N + k)
      end do
    end do
  end do
  call begin_test('5x5_full')
  call print_array('Q', a_r, 2*N*N)
  call print_array('QtQ', qtq_r, 2*N*N)
  call print_int('INFO', INFO)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 2: 5x5 partial range (ILO=2, IHI=4)
  ! ---------------------------------------------------------------
  N = 5; LDA = 5; ILO = 2; IHI = 4
  A = (0.0d0, 0.0d0)
  A(1)  = (2.0d0, 1.0d0);  A(6)  = (1.0d0, -0.5d0); A(11) = (3.0d0, 0.0d0);  A(16) = (1.0d0, 1.0d0);  A(21) = (4.0d0, -1.0d0)
  A(2)  = (1.0d0, 0.5d0);  A(7)  = (4.0d0, 0.0d0);  A(12) = (1.0d0, -1.0d0); A(17) = (2.0d0, 0.0d0);  A(22) = (1.0d0, 0.5d0)
  A(3)  = (3.0d0, -1.0d0); A(8)  = (1.0d0, 1.0d0);  A(13) = (5.0d0, 0.5d0);  A(18) = (1.0d0, -0.5d0); A(23) = (2.0d0, 0.0d0)
  A(4)  = (1.0d0, 0.0d0);  A(9)  = (2.0d0, -0.5d0); A(14) = (1.0d0, 0.0d0);  A(19) = (6.0d0, 1.0d0);  A(24) = (1.0d0, -1.0d0)
  A(5)  = (4.0d0, 1.0d0);  A(10) = (1.0d0, 0.0d0);  A(15) = (2.0d0, 1.0d0);  A(20) = (1.0d0, 0.5d0);  A(25) = (7.0d0, 0.0d0)
  TAU = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0)
  call ZGEHRD(N, ILO, IHI, A, LDA, TAU, WORK, LWORK, INFO)
  call ZUNGHR(N, ILO, IHI, A, LDA, TAU, WORK, LWORK, INFO)
  ! Verify Q^H * Q = I
  QtQ = (0.0d0, 0.0d0)
  do i = 1, N
    do j = 1, N
      do k = 1, N
        QtQ((j-1)*N + i) = QtQ((j-1)*N + i) + conjg(A((i-1)*N + k)) * A((j-1)*N + k)
      end do
    end do
  end do
  call begin_test('5x5_partial')
  call print_array('Q', a_r, 2*N*N)
  call print_array('QtQ', qtq_r, 2*N*N)
  call print_int('INFO', INFO)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 3: N=0 quick return
  ! ---------------------------------------------------------------
  N = 0; ILO = 1; IHI = 0
  A = (0.0d0, 0.0d0); TAU = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0)
  call ZUNGHR(N, ILO, IHI, A, MAXN, TAU, WORK, LWORK, INFO)
  call begin_test('n_zero')
  call print_int('INFO', INFO)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 4: N=1 (trivial, 1x1 identity)
  ! ---------------------------------------------------------------
  N = 1; LDA = 1; ILO = 1; IHI = 1
  A = (0.0d0, 0.0d0); A(1) = (99.0d0, -3.0d0)
  TAU = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0)
  call ZGEHRD(N, ILO, IHI, A, LDA, TAU, WORK, LWORK, INFO)
  call ZUNGHR(N, ILO, IHI, A, LDA, TAU, WORK, LWORK, INFO)
  call begin_test('n_one')
  call print_array('Q', a_r, 2*N*N)
  call print_int('INFO', INFO)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 5: ILO=IHI (no reflectors, should be identity)
  ! ---------------------------------------------------------------
  N = 4; LDA = 4; ILO = 2; IHI = 2
  A = (0.0d0, 0.0d0)
  A(1)  = (1.0d0, 0.0d0); A(5)  = (2.0d0, 1.0d0); A(9)  = (3.0d0, -1.0d0); A(13) = (4.0d0, 0.5d0)
  A(2)  = (0.0d0, 0.0d0); A(6)  = (5.0d0, 0.0d0); A(10) = (6.0d0, 2.0d0);  A(14) = (7.0d0, -0.5d0)
  A(3)  = (0.0d0, 0.0d0); A(7)  = (0.0d0, 0.0d0); A(11) = (8.0d0, 0.0d0);  A(15) = (9.0d0, 1.0d0)
  A(4)  = (0.0d0, 0.0d0); A(8)  = (0.0d0, 0.0d0); A(12) = (0.0d0, 0.0d0);  A(16) = (10.0d0, 0.0d0)
  TAU = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0)
  call ZGEHRD(N, ILO, IHI, A, LDA, TAU, WORK, LWORK, INFO)
  call ZUNGHR(N, ILO, IHI, A, LDA, TAU, WORK, LWORK, INFO)
  call begin_test('ilo_eq_ihi')
  call print_array('Q', a_r, 2*N*N)
  call print_int('INFO', INFO)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 6: 4x4 full range (ILO=1, IHI=4)
  ! ---------------------------------------------------------------
  N = 4; LDA = 4; ILO = 1; IHI = 4
  A = (0.0d0, 0.0d0)
  A(1)  = (1.0d0, 0.5d0);  A(5)  = (2.0d0, -1.0d0); A(9)  = (3.0d0, 0.0d0);  A(13) = (4.0d0, 1.0d0)
  A(2)  = (5.0d0, -0.5d0); A(6)  = (6.0d0, 1.0d0);  A(10) = (7.0d0, -2.0d0); A(14) = (8.0d0, 0.0d0)
  A(3)  = (9.0d0, 1.0d0);  A(7)  = (10.0d0, 0.0d0); A(11) = (11.0d0, 1.0d0); A(15) = (12.0d0, -1.0d0)
  A(4)  = (13.0d0, -1.0d0);A(8)  = (14.0d0, 2.0d0); A(12) = (15.0d0, 0.5d0); A(16) = (16.0d0, 0.0d0)
  TAU = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0)
  call ZGEHRD(N, ILO, IHI, A, LDA, TAU, WORK, LWORK, INFO)
  call ZUNGHR(N, ILO, IHI, A, LDA, TAU, WORK, LWORK, INFO)
  ! Verify Q^H * Q = I
  QtQ = (0.0d0, 0.0d0)
  do i = 1, N
    do j = 1, N
      do k = 1, N
        QtQ((j-1)*N + i) = QtQ((j-1)*N + i) + conjg(A((i-1)*N + k)) * A((j-1)*N + k)
      end do
    end do
  end do
  call begin_test('4x4_full')
  call print_array('Q', a_r, 2*N*N)
  call print_array('QtQ', qtq_r, 2*N*N)
  call print_int('INFO', INFO)
  call end_test()

end program
