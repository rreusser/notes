program test_zsyswapr
  use test_utils
  implicit none

  integer, parameter :: N = 6
  integer, parameter :: LDA = 6

  ! LDA*N = 36 complex = 72 doubles
  double precision :: A_r(72), A0_r(72)
  complex*16 :: A(LDA, N), A0(LDA, N)
  equivalence (A, A_r)
  equivalence (A0, A0_r)
  integer :: i, j

  ! Build a complex symmetric base matrix (A(i,j) = A(j,i)), distinct-valued.
  ! Use A(i,j) = ( 10*min(i,j)+max(i,j), min(i,j) - max(i,j)*0.1d0 )
  do j = 1, N
    do i = 1, N
      A0(i, j) = dcmplx( dble( 10 * min(i,j) + max(i,j) ), &
                         dble( min(i,j) ) - dble( max(i,j) ) * 0.1d0 )
    end do
  end do

  ! =====================================================
  ! Test 1: Upper, non-adjacent swap (i1=2, i2=5)
  ! =====================================================
  A = A0
  call ZSYSWAPR('U', N, A, LDA, 2, 5)
  call begin_test('upper_non_adjacent')
  call print_array('a', A_r, 2*N*N)
  call end_test()

  ! =====================================================
  ! Test 2: Upper, adjacent swap (i1=2, i2=3)
  ! =====================================================
  A = A0
  call ZSYSWAPR('U', N, A, LDA, 2, 3)
  call begin_test('upper_adjacent')
  call print_array('a', A_r, 2*N*N)
  call end_test()

  ! =====================================================
  ! Test 3: Upper, i1 = i2 (no-op)
  ! =====================================================
  A = A0
  call ZSYSWAPR('U', N, A, LDA, 3, 3)
  call begin_test('upper_noop')
  call print_array('a', A_r, 2*N*N)
  call end_test()

  ! =====================================================
  ! Test 4: Upper, i1 = 1 boundary
  ! =====================================================
  A = A0
  call ZSYSWAPR('U', N, A, LDA, 1, 4)
  call begin_test('upper_i1_first')
  call print_array('a', A_r, 2*N*N)
  call end_test()

  ! =====================================================
  ! Test 5: Upper, i2 = N boundary
  ! =====================================================
  A = A0
  call ZSYSWAPR('U', N, A, LDA, 3, N)
  call begin_test('upper_i2_last')
  call print_array('a', A_r, 2*N*N)
  call end_test()

  ! =====================================================
  ! Test 6: Upper, i1=1, i2=N both boundaries
  ! =====================================================
  A = A0
  call ZSYSWAPR('U', N, A, LDA, 1, N)
  call begin_test('upper_both_bounds')
  call print_array('a', A_r, 2*N*N)
  call end_test()

  ! =====================================================
  ! Test 7: Lower, non-adjacent swap (i1=2, i2=5)
  ! =====================================================
  A = A0
  call ZSYSWAPR('L', N, A, LDA, 2, 5)
  call begin_test('lower_non_adjacent')
  call print_array('a', A_r, 2*N*N)
  call end_test()

  ! =====================================================
  ! Test 8: Lower, adjacent swap (i1=2, i2=3)
  ! =====================================================
  A = A0
  call ZSYSWAPR('L', N, A, LDA, 2, 3)
  call begin_test('lower_adjacent')
  call print_array('a', A_r, 2*N*N)
  call end_test()

  ! =====================================================
  ! Test 9: Lower, i1 = i2 (no-op)
  ! =====================================================
  A = A0
  call ZSYSWAPR('L', N, A, LDA, 3, 3)
  call begin_test('lower_noop')
  call print_array('a', A_r, 2*N*N)
  call end_test()

  ! =====================================================
  ! Test 10: Lower, i1=1 boundary
  ! =====================================================
  A = A0
  call ZSYSWAPR('L', N, A, LDA, 1, 4)
  call begin_test('lower_i1_first')
  call print_array('a', A_r, 2*N*N)
  call end_test()

  ! =====================================================
  ! Test 11: Lower, i2=N boundary
  ! =====================================================
  A = A0
  call ZSYSWAPR('L', N, A, LDA, 3, N)
  call begin_test('lower_i2_last')
  call print_array('a', A_r, 2*N*N)
  call end_test()

  ! =====================================================
  ! Test 12: Lower, i1=1, i2=N both boundaries
  ! =====================================================
  A = A0
  call ZSYSWAPR('L', N, A, LDA, 1, N)
  call begin_test('lower_both_bounds')
  call print_array('a', A_r, 2*N*N)
  call end_test()

end program
