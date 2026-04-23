program test_zheswapr
  use test_utils
  implicit none

  integer, parameter :: N = 6
  integer, parameter :: LDA = 6
  complex*16 :: A(LDA, N)
  complex*16 :: A0(LDA, N)
  double precision :: A_r(2*LDA*N)
  equivalence (A, A_r)
  integer :: i, j

  ! Build a Hermitian base matrix with real diagonal and non-trivial
  ! complex off-diagonals. A(i,j) = conj(A(j,i)).
  ! For i<j: A(i,j) = (10*i + j) + (i + j)*sqrt(-1)
  ! For i=j: A(i,i) = real value
  ! For i>j: A(i,j) = conj( A(j,i) )
  do j = 1, N
    do i = 1, N
      if ( i .eq. j ) then
        A0(i,j) = dcmplx( dble( 10*i + 1 ), 0.0d0 )
      else if ( i .lt. j ) then
        A0(i,j) = dcmplx( dble( 10*i + j ), dble( i + j ) )
      else
        A0(i,j) = dcmplx( dble( 10*j + i ), -dble( i + j ) )
      end if
    end do
  end do

  ! =====================================================
  ! Test 1: Upper, non-adjacent swap (i1=2, i2=5)
  ! =====================================================
  A = A0
  call ZHESWAPR('U', N, A, LDA, 2, 5)
  call begin_test('upper_non_adjacent')
  call print_array('a', A_r, 2*N*N)
  call end_test()

  ! =====================================================
  ! Test 2: Upper, adjacent swap (i1=2, i2=3)
  ! =====================================================
  A = A0
  call ZHESWAPR('U', N, A, LDA, 2, 3)
  call begin_test('upper_adjacent')
  call print_array('a', A_r, 2*N*N)
  call end_test()

  ! =====================================================
  ! Test 3: Upper, i1 = i2 no-op
  ! =====================================================
  A = A0
  call ZHESWAPR('U', N, A, LDA, 3, 3)
  call begin_test('upper_noop')
  call print_array('a', A_r, 2*N*N)
  call end_test()

  ! =====================================================
  ! Test 4: Upper, i1 = 1 boundary
  ! =====================================================
  A = A0
  call ZHESWAPR('U', N, A, LDA, 1, 4)
  call begin_test('upper_i1_first')
  call print_array('a', A_r, 2*N*N)
  call end_test()

  ! =====================================================
  ! Test 5: Upper, i2 = N boundary
  ! =====================================================
  A = A0
  call ZHESWAPR('U', N, A, LDA, 3, N)
  call begin_test('upper_i2_last')
  call print_array('a', A_r, 2*N*N)
  call end_test()

  ! =====================================================
  ! Test 6: Upper, i1=1, i2=N both boundaries
  ! =====================================================
  A = A0
  call ZHESWAPR('U', N, A, LDA, 1, N)
  call begin_test('upper_both_bounds')
  call print_array('a', A_r, 2*N*N)
  call end_test()

  ! =====================================================
  ! Test 7: Lower, non-adjacent swap (i1=2, i2=5)
  ! =====================================================
  A = A0
  call ZHESWAPR('L', N, A, LDA, 2, 5)
  call begin_test('lower_non_adjacent')
  call print_array('a', A_r, 2*N*N)
  call end_test()

  ! =====================================================
  ! Test 8: Lower, adjacent swap (i1=2, i2=3)
  ! =====================================================
  A = A0
  call ZHESWAPR('L', N, A, LDA, 2, 3)
  call begin_test('lower_adjacent')
  call print_array('a', A_r, 2*N*N)
  call end_test()

  ! =====================================================
  ! Test 9: Lower, i1 = i2 no-op
  ! =====================================================
  A = A0
  call ZHESWAPR('L', N, A, LDA, 3, 3)
  call begin_test('lower_noop')
  call print_array('a', A_r, 2*N*N)
  call end_test()

  ! =====================================================
  ! Test 10: Lower, i1=1 boundary
  ! =====================================================
  A = A0
  call ZHESWAPR('L', N, A, LDA, 1, 4)
  call begin_test('lower_i1_first')
  call print_array('a', A_r, 2*N*N)
  call end_test()

  ! =====================================================
  ! Test 11: Lower, i2=N boundary
  ! =====================================================
  A = A0
  call ZHESWAPR('L', N, A, LDA, 3, N)
  call begin_test('lower_i2_last')
  call print_array('a', A_r, 2*N*N)
  call end_test()

  ! =====================================================
  ! Test 12: Lower, i1=1, i2=N both boundaries
  ! =====================================================
  A = A0
  call ZHESWAPR('L', N, A, LDA, 1, N)
  call begin_test('lower_both_bounds')
  call print_array('a', A_r, 2*N*N)
  call end_test()

end program
