program test_dgetrf
  use test_utils
  implicit none
  double precision :: a(100)
  integer :: ipiv(10), info

  ! Test 1: 3x3 non-singular matrix
  ! A = [2 1 1; 4 3 3; 8 7 9] col-major
  a = 0.0d0
  a(1) = 2.0d0; a(2) = 4.0d0; a(3) = 8.0d0
  a(4) = 1.0d0; a(5) = 3.0d0; a(6) = 7.0d0
  a(7) = 1.0d0; a(8) = 3.0d0; a(9) = 9.0d0
  ipiv = 0
  call dgetrf(3, 3, a, 3, ipiv, info)
  call begin_test('3x3')
  call print_array('a', a, 9)
  call print_int_array('ipiv', ipiv, 3)
  call print_int('info', info)
  call end_test()

  ! Test 2: 5x4 tall matrix
  a = 0.0d0
  a(1) = 1.0d0; a(2) = 6.0d0; a(3) = 11.0d0; a(4) = 16.0d0; a(5) = 21.0d0
  a(6) = 2.0d0; a(7) = 7.0d0; a(8) = 12.0d0; a(9) = 17.0d0; a(10) = 22.0d0
  a(11) = 3.0d0; a(12) = 8.0d0; a(13) = 13.0d0; a(14) = 18.0d0; a(15) = 23.0d0
  a(16) = 4.0d0; a(17) = 9.0d0; a(18) = 14.0d0; a(19) = 19.0d0; a(20) = 24.0d0
  ipiv = 0
  call dgetrf(5, 4, a, 5, ipiv, info)
  call begin_test('5x4')
  call print_array('a', a, 20)
  call print_int_array('ipiv', ipiv, 4)
  call print_int('info', info)
  call end_test()

  ! Test 3: singular 3x3
  a = 0.0d0
  a(1) = 1.0d0; a(2) = 2.0d0; a(3) = 3.0d0
  a(4) = 2.0d0; a(5) = 4.0d0; a(6) = 6.0d0
  a(7) = 3.0d0; a(8) = 6.0d0; a(9) = 9.0d0
  ipiv = 0
  call dgetrf(3, 3, a, 3, ipiv, info)
  call begin_test('singular')
  call print_array('a', a, 9)
  call print_int_array('ipiv', ipiv, 3)
  call print_int('info', info)
  call end_test()

  ! Test 4: N=0 quick return
  call dgetrf(3, 0, a, 3, ipiv, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 5: M=0 quick return
  call dgetrf(0, 3, a, 1, ipiv, info)
  call begin_test('m_zero')
  call print_int('info', info)
  call end_test()

  ! Test 6: 1x1
  a(1) = 7.0d0
  ipiv = 0
  call dgetrf(1, 1, a, 1, ipiv, info)
  call begin_test('1x1')
  call print_array('a', a, 1)
  call print_int_array('ipiv', ipiv, 1)
  call print_int('info', info)
  call end_test()

  ! Test 7: 4x4 to exercise blocked path (though NB>=min(M,N) means it uses dgetrf2)
  a = 0.0d0
  a(1) = 2.0d0; a(2) = 1.0d0; a(3) = 3.0d0; a(4) = 4.0d0
  a(5) = 1.0d0; a(6) = 3.0d0; a(7) = 2.0d0; a(8) = 1.0d0
  a(9) = 3.0d0; a(10) = 2.0d0; a(11) = 1.0d0; a(12) = 2.0d0
  a(13) = 1.0d0; a(14) = 2.0d0; a(15) = 4.0d0; a(16) = 3.0d0
  ipiv = 0
  call dgetrf(4, 4, a, 4, ipiv, info)
  call begin_test('4x4')
  call print_array('a', a, 16)
  call print_int_array('ipiv', ipiv, 4)
  call print_int('info', info)
  call end_test()

end program
