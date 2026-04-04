program test_dtptri
  use test_utils
  implicit none
  double precision :: ap(100)
  integer :: info, n

  ! =============================================
  ! Packed storage convention (column-major):
  !   Upper: AP(i + j*(j-1)/2) = A(i,j) for 1<=i<=j
  !     Col 1: A(1,1)
  !     Col 2: A(1,2), A(2,2)
  !     Col 3: A(1,3), A(2,3), A(3,3)
  !     etc.
  !   Lower: AP(i + (j-1)*(2*n-j)/2) = A(i,j) for j<=i<=n
  !     Col 1: A(1,1), A(2,1), ..., A(n,1)
  !     Col 2: A(2,2), A(3,2), ..., A(n,2)
  !     etc.
  ! =============================================

  ! Test 1: upper, non-unit, 3x3
  ! A = [2 1 3; 0 4 5; 0 0 6]
  ! Upper packed: [2, 1, 4, 3, 5, 6]
  n = 3
  ap = 0.0d0
  ap(1) = 2.0d0; ap(2) = 1.0d0; ap(3) = 4.0d0
  ap(4) = 3.0d0; ap(5) = 5.0d0; ap(6) = 6.0d0
  call dtptri('U', 'N', n, ap, info)
  call begin_test('upper_nonunit_3x3')
  call print_int('info', info)
  call print_array('ap', ap, n*(n+1)/2)
  call end_test()

  ! Test 2: lower, non-unit, 3x3
  ! L = [2 0 0; 1 4 0; 3 5 6]
  ! Lower packed: [2, 1, 3, 4, 5, 6]
  n = 3
  ap = 0.0d0
  ap(1) = 2.0d0; ap(2) = 1.0d0; ap(3) = 3.0d0
  ap(4) = 4.0d0; ap(5) = 5.0d0; ap(6) = 6.0d0
  call dtptri('L', 'N', n, ap, info)
  call begin_test('lower_nonunit_3x3')
  call print_int('info', info)
  call print_array('ap', ap, n*(n+1)/2)
  call end_test()

  ! Test 3: upper, unit, 3x3
  ! A = [1 1 3; 0 1 5; 0 0 1] (diag treated as 1)
  ! Upper packed: [99, 1, 99, 3, 5, 99] (diag stored but ignored)
  n = 3
  ap = 0.0d0
  ap(1) = 99.0d0; ap(2) = 1.0d0; ap(3) = 99.0d0
  ap(4) = 3.0d0; ap(5) = 5.0d0; ap(6) = 99.0d0
  call dtptri('U', 'U', n, ap, info)
  call begin_test('upper_unit_3x3')
  call print_int('info', info)
  call print_array('ap', ap, n*(n+1)/2)
  call end_test()

  ! Test 4: lower, unit, 3x3
  ! L = [1 0 0; 1 1 0; 3 5 1] (diag treated as 1)
  ! Lower packed: [99, 1, 3, 99, 5, 99]
  n = 3
  ap = 0.0d0
  ap(1) = 99.0d0; ap(2) = 1.0d0; ap(3) = 3.0d0
  ap(4) = 99.0d0; ap(5) = 5.0d0; ap(6) = 99.0d0
  call dtptri('L', 'U', n, ap, info)
  call begin_test('lower_unit_3x3')
  call print_int('info', info)
  call print_array('ap', ap, n*(n+1)/2)
  call end_test()

  ! Test 5: N=0 (quick return)
  call dtptri('U', 'N', 0, ap, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 6: N=1, non-unit
  n = 1
  ap(1) = 4.0d0
  call dtptri('U', 'N', n, ap, info)
  call begin_test('n_one')
  call print_int('info', info)
  call print_array('ap', ap, 1)
  call end_test()

  ! Test 7: upper, non-unit, 4x4
  ! A = [2 1 3 7; 0 4 5 2; 0 0 6 1; 0 0 0 3]
  ! Upper packed: [2, 1, 4, 3, 5, 6, 7, 2, 1, 3]
  n = 4
  ap = 0.0d0
  ap(1) = 2.0d0; ap(2) = 1.0d0; ap(3) = 4.0d0
  ap(4) = 3.0d0; ap(5) = 5.0d0; ap(6) = 6.0d0
  ap(7) = 7.0d0; ap(8) = 2.0d0; ap(9) = 1.0d0; ap(10) = 3.0d0
  call dtptri('U', 'N', n, ap, info)
  call begin_test('upper_nonunit_4x4')
  call print_int('info', info)
  call print_array('ap', ap, n*(n+1)/2)
  call end_test()

  ! Test 8: lower, non-unit, 4x4
  ! L = [3 0 0 0; 1 2 0 0; 4 1 5 0; 2 3 1 4]
  ! Lower packed: [3, 1, 4, 2, 2, 1, 3, 5, 1, 4]
  n = 4
  ap = 0.0d0
  ap(1) = 3.0d0; ap(2) = 1.0d0; ap(3) = 4.0d0; ap(4) = 2.0d0
  ap(5) = 2.0d0; ap(6) = 1.0d0; ap(7) = 3.0d0
  ap(8) = 5.0d0; ap(9) = 1.0d0
  ap(10) = 4.0d0
  call dtptri('L', 'N', n, ap, info)
  call begin_test('lower_nonunit_4x4')
  call print_int('info', info)
  call print_array('ap', ap, n*(n+1)/2)
  call end_test()

  ! Test 9: singular upper (zero on diagonal at position 2)
  ! A = [2 1 0; 0 4 5; 0 0 6] but A(2,2)=0 packed: [2, 1, 0, 3, 5, 6]
  n = 3
  ap = 0.0d0
  ap(1) = 2.0d0; ap(2) = 1.0d0; ap(3) = 0.0d0
  ap(4) = 3.0d0; ap(5) = 5.0d0; ap(6) = 6.0d0
  call dtptri('U', 'N', n, ap, info)
  call begin_test('singular_upper')
  call print_int('info', info)
  call end_test()

  ! Test 10: singular lower (zero on diagonal at position 1)
  ! L(1,1)=0 packed: [0, 1, 3, 4, 5, 6]
  n = 3
  ap = 0.0d0
  ap(1) = 0.0d0; ap(2) = 1.0d0; ap(3) = 3.0d0
  ap(4) = 4.0d0; ap(5) = 5.0d0; ap(6) = 6.0d0
  call dtptri('L', 'N', n, ap, info)
  call begin_test('singular_lower')
  call print_int('info', info)
  call end_test()

  ! Test 11: singular lower at last diagonal position
  ! L(3,3)=0 packed: [2, 1, 3, 4, 5, 0]
  n = 3
  ap = 0.0d0
  ap(1) = 2.0d0; ap(2) = 1.0d0; ap(3) = 3.0d0
  ap(4) = 4.0d0; ap(5) = 5.0d0; ap(6) = 0.0d0
  call dtptri('L', 'N', n, ap, info)
  call begin_test('singular_lower_last')
  call print_int('info', info)
  call end_test()

  ! Test 12: singular upper at first diagonal position
  ! A(1,1)=0 packed: [0, 1, 4, 3, 5, 6]
  n = 3
  ap = 0.0d0
  ap(1) = 0.0d0; ap(2) = 1.0d0; ap(3) = 4.0d0
  ap(4) = 3.0d0; ap(5) = 5.0d0; ap(6) = 6.0d0
  call dtptri('U', 'N', n, ap, info)
  call begin_test('singular_upper_first')
  call print_int('info', info)
  call end_test()

  ! Test 13: identity 3x3 upper
  n = 3
  ap = 0.0d0
  ap(1) = 1.0d0; ap(2) = 0.0d0; ap(3) = 1.0d0
  ap(4) = 0.0d0; ap(5) = 0.0d0; ap(6) = 1.0d0
  call dtptri('U', 'N', n, ap, info)
  call begin_test('identity_upper')
  call print_int('info', info)
  call print_array('ap', ap, n*(n+1)/2)
  call end_test()

end program
