program test_ztzrzf
  use test_utils
  implicit none

  integer, parameter :: NMAX = 200
  complex*16 :: A(NMAX, NMAX), TAU(NMAX), WORK(20000)
  complex*16 :: Apk(NMAX * NMAX), TAUpk(NMAX)
  double precision :: Apk_r(2 * NMAX * NMAX), TAUpk_r(2 * NMAX)
  equivalence (Apk, Apk_r)
  equivalence (TAUpk, TAUpk_r)

  integer :: INFO, i, j, M, N

  ! Test 1: 3x5 upper trapezoidal (unblocked path)
  M = 3; N = 5
  A = (0.0d0, 0.0d0)
  A(1,1) = ( 4.0d0,  0.5d0); A(1,2) = ( 1.0d0, -0.2d0); A(1,3) = ( 2.0d0, 0.3d0)
  A(1,4) = ( 3.0d0,  0.1d0); A(1,5) = ( 1.0d0, -0.4d0)
  A(2,2) = ( 5.0d0,  0.3d0); A(2,3) = ( 1.0d0, 0.1d0)
  A(2,4) = ( 2.0d0,  0.2d0); A(2,5) = ( 4.0d0, -0.5d0)
  A(3,3) = ( 6.0d0, 0.4d0)
  A(3,4) = ( 1.0d0, -0.2d0); A(3,5) = ( 2.0d0,  0.6d0)
  TAU = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0)
  call ZTZRZF(M, N, A, NMAX, TAU, WORK, 20000, INFO)
  call begin_test('3x5')
  do j = 1, N
    do i = 1, M
      Apk(i + (j-1)*M) = A(i, j)
    end do
  end do
  do i = 1, M
    TAUpk(i) = TAU(i)
  end do
  call print_array('A', Apk_r, 2*M*N)
  call print_array('TAU', TAUpk_r, 2*M)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 2: 4x6 upper trapezoidal (unblocked path)
  M = 4; N = 6
  A = (0.0d0, 0.0d0)
  A(1,1) = ( 5.0d0, 0.1d0); A(1,2) = ( 1.0d0, -0.3d0); A(1,3) = ( 2.0d0, 0.5d0)
  A(1,4) = ( 3.0d0, 0.2d0); A(1,5) = ( 1.0d0,  0.4d0); A(1,6) = ( 2.0d0, -0.1d0)
  A(2,2) = ( 6.0d0, -0.2d0); A(2,3) = ( 1.0d0, 0.3d0)
  A(2,4) = ( 2.0d0, -0.4d0); A(2,5) = ( 3.0d0,  0.1d0); A(2,6) = ( 1.0d0, -0.3d0)
  A(3,3) = ( 7.0d0, 0.4d0)
  A(3,4) = ( 1.0d0,  0.2d0); A(3,5) = ( 2.0d0, -0.5d0); A(3,6) = ( 3.0d0, 0.3d0)
  A(4,4) = ( 8.0d0, -0.1d0)
  A(4,5) = ( 1.0d0, 0.6d0); A(4,6) = ( 2.0d0,  0.2d0)
  TAU = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0)
  call ZTZRZF(M, N, A, NMAX, TAU, WORK, 20000, INFO)
  call begin_test('4x6')
  do j = 1, N
    do i = 1, M
      Apk(i + (j-1)*M) = A(i, j)
    end do
  end do
  do i = 1, M
    TAUpk(i) = TAU(i)
  end do
  call print_array('A', Apk_r, 2*M*N)
  call print_array('TAU', TAUpk_r, 2*M)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 3: M = N (square) — TAU should be zero, A unchanged
  M = 3; N = 3
  A = (0.0d0, 0.0d0)
  A(1,1) = ( 3.0d0, 0.5d0); A(1,2) = ( 1.0d0, -0.2d0); A(1,3) = ( 2.0d0, 0.3d0)
  A(2,2) = ( 4.0d0,  0.1d0); A(2,3) = ( 1.0d0, 0.4d0)
  A(3,3) = ( 5.0d0, -0.2d0)
  TAU(1) = (7.0d0, 7.0d0); TAU(2) = (7.0d0, 7.0d0); TAU(3) = (7.0d0, 7.0d0)
  WORK = (0.0d0, 0.0d0)
  call ZTZRZF(M, N, A, NMAX, TAU, WORK, 20000, INFO)
  call begin_test('square_3x3')
  do j = 1, N
    do i = 1, M
      Apk(i + (j-1)*M) = A(i, j)
    end do
  end do
  do i = 1, M
    TAUpk(i) = TAU(i)
  end do
  call print_array('A', Apk_r, 2*M*N)
  call print_array('TAU', TAUpk_r, 2*M)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 4: M = 0 quick return
  TAU = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0)
  call ZTZRZF(0, 5, A, NMAX, TAU, WORK, 20000, INFO)
  call begin_test('m_zero')
  call print_int('INFO', INFO)
  call end_test()

  ! Test 5: 1x4 single row
  M = 1; N = 4
  A = (0.0d0, 0.0d0)
  A(1,1) = (3.0d0, 0.7d0); A(1,2) = (1.0d0, -0.4d0); A(1,3) = (2.0d0, 0.5d0); A(1,4) = (1.0d0, 0.3d0)
  TAU = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0)
  call ZTZRZF(M, N, A, NMAX, TAU, WORK, 20000, INFO)
  call begin_test('1x4')
  do j = 1, N
    do i = 1, M
      Apk(i + (j-1)*M) = A(i, j)
    end do
  end do
  TAUpk(1) = TAU(1)
  call print_array('A', Apk_r, 2*M*N)
  call print_array('TAU', TAUpk_r, 2*M)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 6: 2x4 with L = 2
  M = 2; N = 4
  A = (0.0d0, 0.0d0)
  A(1,1) = ( 2.0d0,  0.2d0); A(1,2) = ( 1.0d0, -0.1d0); A(1,3) = ( 3.0d0, 0.4d0)
  A(1,4) = ( 1.0d0, -0.3d0)
  A(2,2) = ( 3.0d0,  0.3d0); A(2,3) = ( 1.0d0, -0.2d0)
  A(2,4) = ( 2.0d0,  0.5d0)
  TAU = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0)
  call ZTZRZF(M, N, A, NMAX, TAU, WORK, 20000, INFO)
  call begin_test('2x4')
  do j = 1, N
    do i = 1, M
      Apk(i + (j-1)*M) = A(i, j)
    end do
  end do
  do i = 1, M
    TAUpk(i) = TAU(i)
  end do
  call print_array('A', Apk_r, 2*M*N)
  call print_array('TAU', TAUpk_r, 2*M)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 7: Larger matrix to exercise unblocked path (M=40, N=80) — still under NX=128.
  M = 40; N = 80
  A = (0.0d0, 0.0d0)
  do j = 1, N
    do i = 1, M
      if (j .ge. i) then
        if (i .eq. j) then
          A(i,j) = dcmplx(10.0d0 + dble(i), 0.1d0 * dble(i))
        else
          A(i,j) = dcmplx(1.0d0 / dble(j - i + 1), 0.05d0 * dble(j - i))
        end if
      end if
    end do
  end do
  TAU = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0)
  call ZTZRZF(M, N, A, NMAX, TAU, WORK, 20000, INFO)
  call begin_test('large_40x80')
  do j = 1, N
    do i = 1, M
      Apk(i + (j-1)*M) = A(i, j)
    end do
  end do
  do i = 1, M
    TAUpk(i) = TAU(i)
  end do
  call print_array('A', Apk_r, 2*M*N)
  call print_array('TAU', TAUpk_r, 2*M)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 8: M=N=0 quick return
  TAU = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0)
  call ZTZRZF(0, 0, A, NMAX, TAU, WORK, 20000, INFO)
  call begin_test('m_n_zero')
  call print_int('INFO', INFO)
  call end_test()

  ! Test 9: Square M=N=4 — TAU should all be zero
  M = 4; N = 4
  A = (0.0d0, 0.0d0)
  A(1,1) = ( 4.0d0, 0.2d0); A(1,2) = ( 1.0d0, -0.1d0); A(1,3) = ( 2.0d0, 0.3d0); A(1,4) = ( 3.0d0, -0.4d0)
  A(2,2) = ( 5.0d0, 0.4d0); A(2,3) = ( 1.0d0,  0.5d0); A(2,4) = ( 2.0d0, -0.2d0)
  A(3,3) = ( 6.0d0, -0.3d0); A(3,4) = ( 1.0d0,  0.1d0)
  A(4,4) = ( 7.0d0,  0.2d0)
  TAU(1) = (9.9d0, -1.0d0); TAU(2) = (9.9d0, -1.0d0); TAU(3) = (9.9d0, -1.0d0); TAU(4) = (9.9d0, -1.0d0)
  WORK = (0.0d0, 0.0d0)
  call ZTZRZF(M, N, A, NMAX, TAU, WORK, 20000, INFO)
  call begin_test('square_4x4')
  do j = 1, N
    do i = 1, M
      Apk(i + (j-1)*M) = A(i, j)
    end do
  end do
  do i = 1, M
    TAUpk(i) = TAU(i)
  end do
  call print_array('A', Apk_r, 2*M*N)
  call print_array('TAU', TAUpk_r, 2*M)
  call print_int('INFO', INFO)
  call end_test()

end program
