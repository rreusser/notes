program test_zsytrf_rook
  use test_utils
  implicit none

  integer, parameter :: NMAX = 50
  complex*16 :: A(NMAX, NMAX), WORK(NMAX*32)
  double precision :: A_r(2*NMAX*NMAX)
  equivalence (A, A_r)
  integer :: IPIV(NMAX), INFO, n, i, j

  ! Test 1: Upper, 4x4 complex symmetric (A = A^T, no conjugation)
  n = 4
  A = (0.0d0, 0.0d0)
  A(1,1) = (4.0d0, 1.0d0)
  A(1,2) = (1.0d0, 2.0d0)
  A(1,3) = (3.0d0, -1.0d0)
  A(1,4) = (0.5d0, 0.5d0)
  A(2,2) = (5.0d0, -1.0d0)
  A(2,3) = (2.0d0, 1.0d0)
  A(2,4) = (1.0d0, -2.0d0)
  A(3,3) = (7.0d0, 2.0d0)
  A(3,4) = (3.0d0, 1.0d0)
  A(4,4) = (6.0d0, -2.0d0)
  IPIV = 0
  call ZSYTRF_ROOK('U', n, A, NMAX, IPIV, WORK, NMAX*32, INFO)
  call begin_test('upper_4x4')
  call print_int('n', n)
  call print_int('info', INFO)
  call print_array('A', A_r, 2*NMAX*n)
  call print_int_array('ipiv', IPIV, n)
  call end_test()

  ! Test 2: Lower, 4x4 complex symmetric
  n = 4
  A = (0.0d0, 0.0d0)
  A(1,1) = (4.0d0, 1.0d0)
  A(2,1) = (1.0d0, 2.0d0)
  A(2,2) = (5.0d0, -1.0d0)
  A(3,1) = (3.0d0, -1.0d0)
  A(3,2) = (2.0d0, 1.0d0)
  A(3,3) = (7.0d0, 2.0d0)
  A(4,1) = (0.5d0, 0.5d0)
  A(4,2) = (1.0d0, -2.0d0)
  A(4,3) = (3.0d0, 1.0d0)
  A(4,4) = (6.0d0, -2.0d0)
  IPIV = 0
  call ZSYTRF_ROOK('L', n, A, NMAX, IPIV, WORK, NMAX*32, INFO)
  call begin_test('lower_4x4')
  call print_int('n', n)
  call print_int('info', INFO)
  call print_array('A', A_r, 2*NMAX*n)
  call print_int_array('ipiv', IPIV, n)
  call end_test()

  ! Test 3: N=0
  IPIV = 0
  call ZSYTRF_ROOK('U', 0, A, NMAX, IPIV, WORK, NMAX*32, INFO)
  call begin_test('n0')
  call print_int('info', INFO)
  call end_test()

  ! Test 4: N=1
  n = 1
  A(1,1) = (3.0d0, 2.0d0)
  IPIV = 0
  call ZSYTRF_ROOK('U', n, A, NMAX, IPIV, WORK, NMAX*32, INFO)
  call begin_test('n1')
  call print_int('info', INFO)
  call print_array('A', A_r, 2)
  call print_int_array('ipiv', IPIV, n)
  call end_test()

  ! Test 5: Singular upper (zero matrix)
  n = 3
  A = (0.0d0, 0.0d0)
  IPIV = 0
  call ZSYTRF_ROOK('U', n, A, NMAX, IPIV, WORK, NMAX*32, INFO)
  call begin_test('singular_upper')
  call print_int('info', INFO)
  call print_int_array('ipiv', IPIV, n)
  call end_test()

  ! Test 6: Lower, 6x6 indefinite (forces 2x2 pivots; zero diagonal)
  n = 6
  A = (0.0d0, 0.0d0)
  A(2,1) = (1.0d0, 0.5d0)
  A(3,1) = (2.0d0, -1.0d0)
  A(4,1) = (3.0d0, 1.0d0)
  A(5,1) = (1.0d0, 0.0d0)
  A(6,1) = (2.0d0, -1.0d0)
  A(3,2) = (4.0d0, 1.0d0)
  A(4,2) = (1.0d0, 0.5d0)
  A(5,2) = (2.0d0, -1.0d0)
  A(6,2) = (1.0d0, 1.0d0)
  A(4,3) = (5.0d0, -2.0d0)
  A(5,3) = (1.0d0, 0.0d0)
  A(6,3) = (2.0d0, 1.0d0)
  A(5,4) = (3.0d0, 1.0d0)
  A(6,4) = (1.0d0, -1.0d0)
  A(6,5) = (4.0d0, 0.5d0)
  IPIV = 0
  call ZSYTRF_ROOK('L', n, A, NMAX, IPIV, WORK, NMAX*32, INFO)
  call begin_test('lower_6x6_indef')
  call print_int('n', n)
  call print_int('info', INFO)
  call print_array('A', A_r, 2*NMAX*n)
  call print_int_array('ipiv', IPIV, n)
  call end_test()

  ! Test 7: Upper, 6x6 same matrix transposed (upper-stored)
  n = 6
  A = (0.0d0, 0.0d0)
  A(1,2) = (1.0d0, 0.5d0)
  A(1,3) = (2.0d0, -1.0d0)
  A(1,4) = (3.0d0, 1.0d0)
  A(1,5) = (1.0d0, 0.0d0)
  A(1,6) = (2.0d0, -1.0d0)
  A(2,3) = (4.0d0, 1.0d0)
  A(2,4) = (1.0d0, 0.5d0)
  A(2,5) = (2.0d0, -1.0d0)
  A(2,6) = (1.0d0, 1.0d0)
  A(3,4) = (5.0d0, -2.0d0)
  A(3,5) = (1.0d0, 0.0d0)
  A(3,6) = (2.0d0, 1.0d0)
  A(4,5) = (3.0d0, 1.0d0)
  A(4,6) = (1.0d0, -1.0d0)
  A(5,6) = (4.0d0, 0.5d0)
  IPIV = 0
  call ZSYTRF_ROOK('U', n, A, NMAX, IPIV, WORK, NMAX*32, INFO)
  call begin_test('upper_6x6_indef')
  call print_int('n', n)
  call print_int('info', INFO)
  call print_array('A', A_r, 2*NMAX*n)
  call print_int_array('ipiv', IPIV, n)
  call end_test()

  ! Test 8: Larger 40x40 lower (exercises blocked path; NB=32, panel + tail)
  ! NOTE: diagonal varies by index (n+i, 0.5+0.1*i) to avoid tied pivot
  ! magnitudes that diverge between JS/Fortran tie-breakers.
  n = 40
  A = (0.0d0, 0.0d0)
  do j = 1, n
    do i = j, n
      if ( i == j ) then
        A(i,j) = cmplx( dble(3*n + i), 0.5d0 + 0.1d0*dble(i), kind=8 )
      else
        A(i,j) = cmplx( dble( mod(i+j, 7) - 3 ), dble( mod(i*j, 5) - 2 ), kind=8 )
      end if
    end do
  end do
  IPIV = 0
  call ZSYTRF_ROOK('L', n, A, NMAX, IPIV, WORK, NMAX*32, INFO)
  call begin_test('lower_40x40_blocked')
  call print_int('n', n)
  call print_int('info', INFO)
  call print_int_array('ipiv', IPIV, n)
  call end_test()

  ! Test 9: 40x40 upper (blocked path, upper case)
  n = 40
  A = (0.0d0, 0.0d0)
  do j = 1, n
    do i = 1, j
      if ( i == j ) then
        A(i,j) = cmplx( dble(3*n + i), 0.5d0 + 0.1d0*dble(i), kind=8 )
      else
        A(i,j) = cmplx( dble( mod(i+j, 7) - 3 ), dble( mod(i*j, 5) - 2 ), kind=8 )
      end if
    end do
  end do
  IPIV = 0
  call ZSYTRF_ROOK('U', n, A, NMAX, IPIV, WORK, NMAX*32, INFO)
  call begin_test('upper_40x40_blocked')
  call print_int('n', n)
  call print_int('info', INFO)
  call print_int_array('ipiv', IPIV, n)
  call end_test()

  ! Test 10: 40x40 zero-diagonal lower (forces many 2x2 pivots in blocked path)
  n = 40
  A = (0.0d0, 0.0d0)
  do j = 1, n
    do i = j+1, n
      A(i,j) = cmplx( sin( dble(i)*0.7d0 + dble(j)*1.3d0 ), &
                      cos( dble(i)*0.4d0 - dble(j)*0.9d0 ), kind=8 )
    end do
  end do
  IPIV = 0
  call ZSYTRF_ROOK('L', n, A, NMAX, IPIV, WORK, NMAX*32, INFO)
  call begin_test('lower_40x40_indef_blocked')
  call print_int('n', n)
  call print_int('info', INFO)
  call print_int_array('ipiv', IPIV, n)
  call end_test()

  ! Test 11: 40x40 zero-diagonal upper (forces many 2x2 pivots, upper)
  n = 40
  A = (0.0d0, 0.0d0)
  do j = 1, n
    do i = 1, j-1
      A(i,j) = cmplx( sin( dble(i)*0.7d0 + dble(j)*1.3d0 ), &
                      cos( dble(i)*0.4d0 - dble(j)*0.9d0 ), kind=8 )
    end do
  end do
  IPIV = 0
  call ZSYTRF_ROOK('U', n, A, NMAX, IPIV, WORK, NMAX*32, INFO)
  call begin_test('upper_40x40_indef_blocked')
  call print_int('n', n)
  call print_int('info', INFO)
  call print_int_array('ipiv', IPIV, n)
  call end_test()

end program
