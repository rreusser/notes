program test_dpptri
  use test_utils
  implicit none

  ! dpptri computes the inverse of a real symmetric positive definite
  ! matrix A using the Cholesky factorization A = U^T*U or A = L*L^T
  ! computed by DPPTRF. The matrix is stored in packed format.
  !
  ! Packed storage convention (column-major):
  !   Upper: AP(i + j*(j-1)/2) = A(i,j) for 1<=i<=j
  !   Lower: AP(i + (j-1)*(2*n-j)/2) = A(i,j) for j<=i<=n

  double precision :: ap(50)
  integer :: info, n

  ! Test 1: N=0 (quick return)
  info = -999
  call dpptri('U', 0, ap, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 2: N=1, upper
  ! A = [4.0], Cholesky: U = [2.0], inv(A) = [0.25]
  ap(1) = 4.0d0
  call dpptrf('U', 1, ap, info)
  call dpptri('U', 1, ap, info)
  call begin_test('n_one_upper')
  call print_int('info', info)
  call print_array('ap', ap, 1)
  call end_test()

  ! Test 3: N=1, lower
  ! A = [9.0], Cholesky: L = [3.0], inv(A) = [1/9]
  ap(1) = 9.0d0
  call dpptrf('L', 1, ap, info)
  call dpptri('L', 1, ap, info)
  call begin_test('n_one_lower')
  call print_int('info', info)
  call print_array('ap', ap, 1)
  call end_test()

  ! Test 4: N=2, upper, basic SPD
  ! A = [4 2; 2 5]
  ! Upper packed: [4, 2, 5]
  ap = 0.0d0
  ap(1) = 4.0d0
  ap(2) = 2.0d0
  ap(3) = 5.0d0
  n = 2
  call dpptrf('U', n, ap, info)
  call dpptri('U', n, ap, info)
  call begin_test('upper_2x2')
  call print_int('info', info)
  call print_array('ap', ap, n*(n+1)/2)
  call end_test()

  ! Test 5: N=2, lower, basic SPD
  ! A = [4 2; 2 5]
  ! Lower packed: [4, 2, 5]
  ap = 0.0d0
  ap(1) = 4.0d0
  ap(2) = 2.0d0
  ap(3) = 5.0d0
  n = 2
  call dpptrf('L', n, ap, info)
  call dpptri('L', n, ap, info)
  call begin_test('lower_2x2')
  call print_int('info', info)
  call print_array('ap', ap, n*(n+1)/2)
  call end_test()

  ! Test 6: N=3, upper, identity
  ! A = I, inv(A) = I
  ap = 0.0d0
  ap(1) = 1.0d0
  ap(2) = 0.0d0
  ap(3) = 1.0d0
  ap(4) = 0.0d0
  ap(5) = 0.0d0
  ap(6) = 1.0d0
  n = 3
  call dpptrf('U', n, ap, info)
  call dpptri('U', n, ap, info)
  call begin_test('identity_upper')
  call print_int('info', info)
  call print_array('ap', ap, n*(n+1)/2)
  call end_test()

  ! Test 7: N=3, lower, identity
  ap = 0.0d0
  ap(1) = 1.0d0
  ap(2) = 0.0d0
  ap(3) = 0.0d0
  ap(4) = 1.0d0
  ap(5) = 0.0d0
  ap(6) = 1.0d0
  n = 3
  call dpptrf('L', n, ap, info)
  call dpptri('L', n, ap, info)
  call begin_test('identity_lower')
  call print_int('info', info)
  call print_array('ap', ap, n*(n+1)/2)
  call end_test()

  ! Test 8: N=3, upper, well-conditioned SPD
  ! A = [25  5  -5;  5  10  2;  -5  2  6]
  ! Upper packed: [25, 5, 10, -5, 2, 6]
  ap = 0.0d0
  ap(1) = 25.0d0
  ap(2) = 5.0d0
  ap(3) = 10.0d0
  ap(4) = -5.0d0
  ap(5) = 2.0d0
  ap(6) = 6.0d0
  n = 3
  call dpptrf('U', n, ap, info)
  call dpptri('U', n, ap, info)
  call begin_test('upper_3x3')
  call print_int('info', info)
  call print_array('ap', ap, n*(n+1)/2)
  call end_test()

  ! Test 9: N=3, lower, same SPD matrix
  ! Lower packed: [25, 5, -5, 10, 2, 6]
  ap = 0.0d0
  ap(1) = 25.0d0
  ap(2) = 5.0d0
  ap(3) = -5.0d0
  ap(4) = 10.0d0
  ap(5) = 2.0d0
  ap(6) = 6.0d0
  n = 3
  call dpptrf('L', n, ap, info)
  call dpptri('L', n, ap, info)
  call begin_test('lower_3x3')
  call print_int('info', info)
  call print_array('ap', ap, n*(n+1)/2)
  call end_test()

  ! Test 10: N=4, upper, SPD matrix
  ! A = [4  2  1  0.5;  2  5  1  0.5;  1  1  5  1;  0.5  0.5  1  5]
  ! Upper packed: [4, 2, 5, 1, 1, 5, 0.5, 0.5, 1, 5]
  ap = 0.0d0
  ap(1) = 4.0d0
  ap(2) = 2.0d0
  ap(3) = 5.0d0
  ap(4) = 1.0d0
  ap(5) = 1.0d0
  ap(6) = 5.0d0
  ap(7) = 0.5d0
  ap(8) = 0.5d0
  ap(9) = 1.0d0
  ap(10) = 5.0d0
  n = 4
  call dpptrf('U', n, ap, info)
  call dpptri('U', n, ap, info)
  call begin_test('upper_4x4')
  call print_int('info', info)
  call print_array('ap', ap, n*(n+1)/2)
  call end_test()

  ! Test 11: N=4, lower, same SPD matrix
  ! Lower packed: [4, 2, 1, 0.5, 5, 1, 0.5, 5, 1, 5]
  ap = 0.0d0
  ap(1) = 4.0d0
  ap(2) = 2.0d0
  ap(3) = 1.0d0
  ap(4) = 0.5d0
  ap(5) = 5.0d0
  ap(6) = 1.0d0
  ap(7) = 0.5d0
  ap(8) = 5.0d0
  ap(9) = 1.0d0
  ap(10) = 5.0d0
  n = 4
  call dpptrf('L', n, ap, info)
  call dpptri('L', n, ap, info)
  call begin_test('lower_4x4')
  call print_int('info', info)
  call print_array('ap', ap, n*(n+1)/2)
  call end_test()

  ! Test 12: singular factor — dtptri returns info > 0
  ! Feed dpptri a pre-factored matrix with a zero on the diagonal
  ! This simulates a singular Cholesky factor
  ap = 0.0d0
  ap(1) = 1.0d0
  ap(2) = 0.0d0
  ap(3) = 0.0d0  ! zero diagonal at position 2
  n = 2
  ! Skip dpptrf — directly call dpptri with a "factor" that has zero diagonal
  call dpptri('U', n, ap, info)
  call begin_test('singular_factor_upper')
  call print_int('info', info)
  call end_test()

  ! Test 13: singular factor, lower
  ap = 0.0d0
  ap(1) = 0.0d0  ! zero diagonal at position 1
  ap(2) = 1.0d0
  ap(3) = 2.0d0
  n = 2
  call dpptri('L', n, ap, info)
  call begin_test('singular_factor_lower')
  call print_int('info', info)
  call end_test()

  ! Test 14: N=5, upper, larger SPD
  ! A = diag(10,10,10,10,10) + 1*ones(5,5) (diagonally dominant)
  ! Upper packed storage of the 5x5 matrix:
  !   col1: A(1,1)=11
  !   col2: A(1,2)=1, A(2,2)=11
  !   col3: A(1,3)=1, A(2,3)=1, A(3,3)=11
  !   col4: A(1,4)=1, A(2,4)=1, A(3,4)=1, A(4,4)=11
  !   col5: A(1,5)=1, A(2,5)=1, A(3,5)=1, A(4,5)=1, A(5,5)=11
  ap = 0.0d0
  ap(1) = 11.0d0
  ap(2) = 1.0d0; ap(3) = 11.0d0
  ap(4) = 1.0d0; ap(5) = 1.0d0; ap(6) = 11.0d0
  ap(7) = 1.0d0; ap(8) = 1.0d0; ap(9) = 1.0d0; ap(10) = 11.0d0
  ap(11) = 1.0d0; ap(12) = 1.0d0; ap(13) = 1.0d0; ap(14) = 1.0d0
  ap(15) = 11.0d0
  n = 5
  call dpptrf('U', n, ap, info)
  call dpptri('U', n, ap, info)
  call begin_test('upper_5x5')
  call print_int('info', info)
  call print_array('ap', ap, n*(n+1)/2)
  call end_test()

  ! Test 15: N=5, lower, same SPD
  ap = 0.0d0
  ap(1) = 11.0d0
  ap(2) = 1.0d0; ap(3) = 1.0d0; ap(4) = 1.0d0; ap(5) = 1.0d0
  ap(6) = 11.0d0
  ap(7) = 1.0d0; ap(8) = 1.0d0; ap(9) = 1.0d0
  ap(10) = 11.0d0
  ap(11) = 1.0d0; ap(12) = 1.0d0
  ap(13) = 11.0d0
  ap(14) = 1.0d0
  ap(15) = 11.0d0
  n = 5
  call dpptrf('L', n, ap, info)
  call dpptri('L', n, ap, info)
  call begin_test('lower_5x5')
  call print_int('info', info)
  call print_array('ap', ap, n*(n+1)/2)
  call end_test()

end program
