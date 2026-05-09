program test_zsytrf_aa
  use test_utils
  implicit none

  ! Per-test array declarations sized exactly to the test's LDA so the
  ! EQUIVALENCE between complex and Float64 views has no padding stride.
  complex*16 :: a4(4, 4)
  double precision :: a4_r(32)
  equivalence (a4, a4_r)
  complex*16 :: a5(5, 5)
  double precision :: a5_r(50)
  equivalence (a5, a5_r)
  complex*16 :: a1(1, 1)
  double precision :: a1_r(2)
  equivalence (a1, a1_r)
  complex*16 :: a40(40, 40)
  double precision :: a40_r(3200)
  equivalence (a40, a40_r)
  complex*16 :: a70(70, 70)
  double precision :: a70_r(9800)
  equivalence (a70, a70_r)
  complex*16 :: work(20000)
  integer :: ipiv(100), info
  integer :: i, j

  ! Test 1: 4x4 symmetric (well-conditioned), UPLO='L'
  a4 = (0.0d0, 0.0d0)
  a4(1,1) = (4.0d0, 0.1d0)
  a4(2,1) = (2.0d0, -0.2d0)
  a4(3,1) = (1.0d0, 0.3d0)
  a4(4,1) = (0.0d0, 0.0d0)
  a4(2,2) = (5.0d0, -0.1d0)
  a4(3,2) = (2.0d0, 0.4d0)
  a4(4,2) = (1.0d0, 0.0d0)
  a4(3,3) = (6.0d0, 0.2d0)
  a4(4,3) = (3.0d0, -0.3d0)
  a4(4,4) = (8.0d0, 0.1d0)
  ipiv = 0
  call zsytrf_aa('L', 4, a4, 4, ipiv, work, 2000, info)
  call begin_test('4x4_lower')
  call print_array('a', a4_r, 32)
  call print_int_array('ipiv', ipiv, 4)
  call print_int('info', info)
  call end_test()

  ! Test 2: 4x4 symmetric (well-conditioned), UPLO='U'
  a4 = (0.0d0, 0.0d0)
  a4(1,1) = (4.0d0, 0.1d0)
  a4(1,2) = (2.0d0, -0.2d0)
  a4(2,2) = (5.0d0, -0.1d0)
  a4(1,3) = (1.0d0, 0.3d0)
  a4(2,3) = (2.0d0, 0.4d0)
  a4(3,3) = (6.0d0, 0.2d0)
  a4(1,4) = (0.0d0, 0.0d0)
  a4(2,4) = (1.0d0, 0.0d0)
  a4(3,4) = (3.0d0, -0.3d0)
  a4(4,4) = (8.0d0, 0.1d0)
  ipiv = 0
  call zsytrf_aa('U', 4, a4, 4, ipiv, work, 2000, info)
  call begin_test('4x4_upper')
  call print_array('a', a4_r, 32)
  call print_int_array('ipiv', ipiv, 4)
  call print_int('info', info)
  call end_test()

  ! Test 3: 4x4 indefinite (sparser), UPLO='L'
  a4 = (0.0d0, 0.0d0)
  a4(1,1) = (0.0d0, 0.0d0)
  a4(2,1) = (1.0d0, 0.5d0)
  a4(3,1) = (2.0d0, -0.3d0)
  a4(4,1) = (3.0d0, 0.2d0)
  a4(2,2) = (0.0d0, 0.0d0)
  a4(3,2) = (4.0d0, 0.1d0)
  a4(4,2) = (5.0d0, -0.4d0)
  a4(3,3) = (0.0d0, 0.0d0)
  a4(4,3) = (6.0d0, 0.3d0)
  a4(4,4) = (0.0d0, 0.0d0)
  ipiv = 0
  call zsytrf_aa('L', 4, a4, 4, ipiv, work, 2000, info)
  call begin_test('4x4_indef_lower')
  call print_array('a', a4_r, 32)
  call print_int_array('ipiv', ipiv, 4)
  call print_int('info', info)
  call end_test()

  ! Test 4: 4x4 indefinite (sparser), UPLO='U'
  a4 = (0.0d0, 0.0d0)
  a4(1,1) = (0.0d0, 0.0d0)
  a4(1,2) = (1.0d0, 0.5d0)
  a4(2,2) = (0.0d0, 0.0d0)
  a4(1,3) = (2.0d0, -0.3d0)
  a4(2,3) = (4.0d0, 0.1d0)
  a4(3,3) = (0.0d0, 0.0d0)
  a4(1,4) = (3.0d0, 0.2d0)
  a4(2,4) = (5.0d0, -0.4d0)
  a4(3,4) = (6.0d0, 0.3d0)
  a4(4,4) = (0.0d0, 0.0d0)
  ipiv = 0
  call zsytrf_aa('U', 4, a4, 4, ipiv, work, 2000, info)
  call begin_test('4x4_indef_upper')
  call print_array('a', a4_r, 32)
  call print_int_array('ipiv', ipiv, 4)
  call print_int('info', info)
  call end_test()

  ! Test 5: N=0
  ipiv = 0
  call zsytrf_aa('L', 0, a1, 1, ipiv, work, 2000, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 6: N=1
  a1(1,1) = (7.0d0, -0.5d0)
  ipiv = 0
  call zsytrf_aa('L', 1, a1, 1, ipiv, work, 2000, info)
  call begin_test('n_one')
  call print_array('a', a1_r, 2)
  call print_int_array('ipiv', ipiv, 1)
  call print_int('info', info)
  call end_test()

  ! Test 7: 5x5 lower
  a5 = (0.0d0, 0.0d0)
  a5(1,1) = (1.0d0, 0.1d0)
  a5(2,1) = (-2.0d0, 0.2d0)
  a5(3,1) = (0.0d0, 0.0d0)
  a5(4,1) = (3.0d0, -0.1d0)
  a5(5,1) = (1.0d0, 0.4d0)
  a5(2,2) = (0.0d0, 0.0d0)
  a5(3,2) = (4.0d0, -0.3d0)
  a5(4,2) = (-1.0d0, 0.5d0)
  a5(5,2) = (2.0d0, 0.0d0)
  a5(3,3) = (-3.0d0, 0.2d0)
  a5(4,3) = (2.0d0, -0.1d0)
  a5(5,3) = (0.0d0, 0.0d0)
  a5(4,4) = (1.0d0, 0.3d0)
  a5(5,4) = (-2.0d0, 0.0d0)
  a5(5,5) = (4.0d0, -0.2d0)
  ipiv = 0
  call zsytrf_aa('L', 5, a5, 5, ipiv, work, 2000, info)
  call begin_test('5x5_lower')
  call print_array('a', a5_r, 50)
  call print_int_array('ipiv', ipiv, 5)
  call print_int('info', info)
  call end_test()

  ! Test 8: 5x5 upper
  a5 = (0.0d0, 0.0d0)
  a5(1,1) = (1.0d0, 0.1d0)
  a5(1,2) = (-2.0d0, 0.2d0)
  a5(2,2) = (0.0d0, 0.0d0)
  a5(1,3) = (0.0d0, 0.0d0)
  a5(2,3) = (4.0d0, -0.3d0)
  a5(3,3) = (-3.0d0, 0.2d0)
  a5(1,4) = (3.0d0, -0.1d0)
  a5(2,4) = (-1.0d0, 0.5d0)
  a5(3,4) = (2.0d0, -0.1d0)
  a5(4,4) = (1.0d0, 0.3d0)
  a5(1,5) = (1.0d0, 0.4d0)
  a5(2,5) = (2.0d0, 0.0d0)
  a5(3,5) = (0.0d0, 0.0d0)
  a5(4,5) = (-2.0d0, 0.0d0)
  a5(5,5) = (4.0d0, -0.2d0)
  ipiv = 0
  call zsytrf_aa('U', 5, a5, 5, ipiv, work, 2000, info)
  call begin_test('5x5_upper')
  call print_array('a', a5_r, 50)
  call print_int_array('ipiv', ipiv, 5)
  call print_int('info', info)
  call end_test()

  ! Test 9: 40x40 lower (exercise blocked path with default NB=32)
  ! Use a diagonally-dominant complex symmetric matrix.
  a40 = (0.0d0, 0.0d0)
  do j = 1, 40
    do i = j, 40
      if ( i .eq. j ) then
        a40(i, j) = cmplx( 3.0d0 * 40.0d0, 0.5d0, kind=8 )
      else
        a40(i, j) = cmplx( dble( mod( (i + j), 7 ) - 3 ), &
                           dble( mod( (i + 2*j), 5 ) - 2 ) * 0.1d0, kind=8 )
      end if
    end do
  end do
  ipiv = 0
  call zsytrf_aa('L', 40, a40, 40, ipiv, work, 2000, info)
  call begin_test('40x40_lower')
  call print_array('a', a40_r, 3200)
  call print_int_array('ipiv', ipiv, 40)
  call print_int('info', info)
  call end_test()

  ! Test 10: 40x40 upper
  a40 = (0.0d0, 0.0d0)
  do j = 1, 40
    do i = 1, j
      if ( i .eq. j ) then
        a40(i, j) = cmplx( 3.0d0 * 40.0d0, 0.5d0, kind=8 )
      else
        a40(i, j) = cmplx( dble( mod( (i + j), 7 ) - 3 ), &
                           dble( mod( (i + 2*j), 5 ) - 2 ) * 0.1d0, kind=8 )
      end if
    end do
  end do
  ipiv = 0
  call zsytrf_aa('U', 40, a40, 40, ipiv, work, 2000, info)
  call begin_test('40x40_upper')
  call print_array('a', a40_r, 3200)
  call print_int_array('ipiv', ipiv, 40)
  call print_int('info', info)
  call end_test()

  ! Test 11: 70x70 lower (3+ panels with NB=32 → J1>1 trailing-update branch)
  a70 = (0.0d0, 0.0d0)
  do j = 1, 70
    do i = j, 70
      if ( i .eq. j ) then
        a70(i, j) = cmplx( 5.0d0 * 70.0d0, 0.5d0, kind=8 )
      else
        a70(i, j) = cmplx( dble( mod( (i + j), 7 ) - 3 ), &
                           dble( mod( (i + 2*j), 5 ) - 2 ) * 0.1d0, kind=8 )
      end if
    end do
  end do
  ipiv = 0
  call zsytrf_aa('L', 70, a70, 70, ipiv, work, 20000, info)
  call begin_test('70x70_lower')
  call print_array('a', a70_r, 9800)
  call print_int_array('ipiv', ipiv, 70)
  call print_int('info', info)
  call end_test()

  ! Test 12: 70x70 upper (3+ panels with NB=32 → J1>1 trailing-update branch)
  a70 = (0.0d0, 0.0d0)
  do j = 1, 70
    do i = 1, j
      if ( i .eq. j ) then
        a70(i, j) = cmplx( 5.0d0 * 70.0d0, 0.5d0, kind=8 )
      else
        a70(i, j) = cmplx( dble( mod( (i + j), 7 ) - 3 ), &
                           dble( mod( (i + 2*j), 5 ) - 2 ) * 0.1d0, kind=8 )
      end if
    end do
  end do
  ipiv = 0
  call zsytrf_aa('U', 70, a70, 70, ipiv, work, 20000, info)
  call begin_test('70x70_upper')
  call print_array('a', a70_r, 9800)
  call print_int_array('ipiv', ipiv, 70)
  call print_int('info', info)
  call end_test()

end program
