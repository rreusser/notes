program test_dgeqrt
  use test_utils
  implicit none

  ! Use a single large allocation; pack matrices with exact dims before printing
  ! to avoid leading-dimension/EQUIVALENCE mismatches.
  integer, parameter :: NMAX = 70
  integer, parameter :: NBMAX = 32
  double precision :: A(NMAX, NMAX), T(NBMAX, NMAX), WORK(NMAX*NMAX)
  double precision :: Apk(NMAX*NMAX), Tpk(NBMAX*NMAX)
  integer :: INFO, i, j, M, N, NB

  ! Test 1: 4x3 with NB=2 (forces blocked path)
  A = 0.0d0; T = 0.0d0; WORK = 0.0d0
  A(1,1) = 2.0d0; A(1,2) = 1.0d0; A(1,3) = 3.0d0
  A(2,1) = 1.0d0; A(2,2) = 4.0d0; A(2,3) = 2.0d0
  A(3,1) = 3.0d0; A(3,2) = 2.0d0; A(3,3) = 5.0d0
  A(4,1) = 1.0d0; A(4,2) = 3.0d0; A(4,3) = 1.0d0
  M = 4; N = 3; NB = 2
  call DGEQRT(M, N, NB, A, NMAX, T, NBMAX, WORK, INFO)
  call begin_test('m4_n3_nb2')
  call pack_matrix(A, NMAX, M, N, Apk)
  call print_array('A', Apk, M*N)
  call pack_matrix(T, NBMAX, NB, min(M,N), Tpk)
  call print_array('T', Tpk, NB*min(M,N))
  call print_int('INFO', INFO)
  call end_test()

  ! Test 2: M=N=3, NB=3 (single block)
  A = 0.0d0; T = 0.0d0; WORK = 0.0d0
  A(1,1) = 4.0d0; A(1,2) = 1.0d0; A(1,3) = 2.0d0
  A(2,1) = 1.0d0; A(2,2) = 5.0d0; A(2,3) = 3.0d0
  A(3,1) = 2.0d0; A(3,2) = 3.0d0; A(3,3) = 6.0d0
  M = 3; N = 3; NB = 3
  call DGEQRT(M, N, NB, A, NMAX, T, NBMAX, WORK, INFO)
  call begin_test('m3_n3_nb3')
  call pack_matrix(A, NMAX, M, N, Apk)
  call print_array('A', Apk, M*N)
  call pack_matrix(T, NBMAX, NB, min(M,N), Tpk)
  call print_array('T', Tpk, NB*min(M,N))
  call print_int('INFO', INFO)
  call end_test()

  ! Test 3: 5x2, NB=1 (each reflector its own block)
  A = 0.0d0; T = 0.0d0; WORK = 0.0d0
  A(1,1) = 1.0d0; A(1,2) = 2.0d0
  A(2,1) = 3.0d0; A(2,2) = 4.0d0
  A(3,1) = 5.0d0; A(3,2) = 6.0d0
  A(4,1) = 2.0d0; A(4,2) = 1.0d0
  A(5,1) = 4.0d0; A(5,2) = 3.0d0
  M = 5; N = 2; NB = 1
  call DGEQRT(M, N, NB, A, NMAX, T, NBMAX, WORK, INFO)
  call begin_test('m5_n2_nb1')
  call pack_matrix(A, NMAX, M, N, Apk)
  call print_array('A', Apk, M*N)
  call pack_matrix(T, NBMAX, NB, min(M,N), Tpk)
  call print_array('T', Tpk, NB*min(M,N))
  call print_int('INFO', INFO)
  call end_test()

  ! Test 4: M=1, N=1
  A = 0.0d0; T = 0.0d0; WORK = 0.0d0
  A(1,1) = 7.5d0
  M = 1; N = 1; NB = 1
  call DGEQRT(M, N, NB, A, NMAX, T, NBMAX, WORK, INFO)
  call begin_test('m1_n1_nb1')
  call pack_matrix(A, NMAX, M, N, Apk)
  call print_array('A', Apk, M*N)
  call pack_matrix(T, NBMAX, NB, min(M,N), Tpk)
  call print_array('T', Tpk, NB*min(M,N))
  call print_int('INFO', INFO)
  call end_test()

  ! Test 5: 3x1, NB=1
  A = 0.0d0; T = 0.0d0; WORK = 0.0d0
  A(1,1) = 1.0d0
  A(2,1) = 2.0d0
  A(3,1) = 2.0d0
  M = 3; N = 1; NB = 1
  call DGEQRT(M, N, NB, A, NMAX, T, NBMAX, WORK, INFO)
  call begin_test('m3_n1_nb1')
  call pack_matrix(A, NMAX, M, N, Apk)
  call print_array('A', Apk, M*N)
  call pack_matrix(T, NBMAX, NB, min(M,N), Tpk)
  call print_array('T', Tpk, NB*min(M,N))
  call print_int('INFO', INFO)
  call end_test()

  ! Test 6: M=4, N=4, NB=2 (multiple equal blocks)
  A = 0.0d0; T = 0.0d0; WORK = 0.0d0
  A(1,1) = 2.0d0; A(1,2) = 0.5d0; A(1,3) = 1.0d0; A(1,4) = 0.3d0
  A(2,1) = 1.0d0; A(2,2) = 3.0d0; A(2,3) = 0.7d0; A(2,4) = 0.4d0
  A(3,1) = 0.5d0; A(3,2) = 0.8d0; A(3,3) = 4.0d0; A(3,4) = 0.6d0
  A(4,1) = 0.2d0; A(4,2) = 0.9d0; A(4,3) = 1.2d0; A(4,4) = 5.0d0
  M = 4; N = 4; NB = 2
  call DGEQRT(M, N, NB, A, NMAX, T, NBMAX, WORK, INFO)
  call begin_test('m4_n4_nb2')
  call pack_matrix(A, NMAX, M, N, Apk)
  call print_array('A', Apk, M*N)
  call pack_matrix(T, NBMAX, NB, min(M,N), Tpk)
  call print_array('T', Tpk, NB*min(M,N))
  call print_int('INFO', INFO)
  call end_test()

  ! Test 7: M=6, N=4, NB=3 (last block smaller than NB)
  A = 0.0d0; T = 0.0d0; WORK = 0.0d0
  do j = 1, 4
    do i = 1, 6
      if (i .eq. j) then
        A(i,j) = 5.0d0 + dble(j)
      else
        A(i,j) = 1.0d0 / dble(abs(i - j) + 1)
      end if
    end do
  end do
  M = 6; N = 4; NB = 3
  call DGEQRT(M, N, NB, A, NMAX, T, NBMAX, WORK, INFO)
  call begin_test('m6_n4_nb3')
  call pack_matrix(A, NMAX, M, N, Apk)
  call print_array('A', Apk, M*N)
  call pack_matrix(T, NBMAX, NB, min(M,N), Tpk)
  call print_array('T', Tpk, NB*min(M,N))
  call print_int('INFO', INFO)
  call end_test()

  ! Test 8: M=3, N=5, NB=2 (wide matrix M < N)
  A = 0.0d0; T = 0.0d0; WORK = 0.0d0
  A(1,1) = 3.0d0; A(1,2) = 1.0d0; A(1,3) = 2.0d0; A(1,4) = 0.5d0; A(1,5) = 0.3d0
  A(2,1) = 1.0d0; A(2,2) = 4.0d0; A(2,3) = 0.7d0; A(2,4) = 1.5d0; A(2,5) = 0.4d0
  A(3,1) = 0.5d0; A(3,2) = 0.8d0; A(3,3) = 5.0d0; A(3,4) = 0.6d0; A(3,5) = 1.1d0
  M = 3; N = 5; NB = 2
  call DGEQRT(M, N, NB, A, NMAX, T, NBMAX, WORK, INFO)
  call begin_test('m3_n5_nb2')
  call pack_matrix(A, NMAX, M, N, Apk)
  call print_array('A', Apk, M*N)
  call pack_matrix(T, NBMAX, NB, min(M,N), Tpk)
  call print_array('T', Tpk, NB*min(M,N))
  call print_int('INFO', INFO)
  call end_test()

  ! Test 9: M=0 edge (quick return)
  A = 0.0d0; T = 0.0d0; WORK = 0.0d0
  M = 0; N = 3; NB = 1
  call DGEQRT(M, N, NB, A, NMAX, T, NBMAX, WORK, INFO)
  call begin_test('m0_n3')
  call print_int('INFO', INFO)
  call end_test()

  ! Test 10: N=0 edge (quick return)
  A = 0.0d0; T = 0.0d0; WORK = 0.0d0
  M = 3; N = 0; NB = 1
  call DGEQRT(M, N, NB, A, NMAX, T, NBMAX, WORK, INFO)
  call begin_test('m3_n0')
  call print_int('INFO', INFO)
  call end_test()

contains

  subroutine pack_matrix(src, lds, mr, nc, dst)
    integer, intent(in) :: lds, mr, nc
    double precision, intent(in)  :: src(lds, *)
    double precision, intent(out) :: dst(*)
    integer :: ii, jj
    do jj = 1, nc
      do ii = 1, mr
        dst(ii + (jj-1)*mr) = src(ii, jj)
      end do
    end do
  end subroutine pack_matrix

end program
