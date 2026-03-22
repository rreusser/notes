program test_zlanhs
  use test_utils
  implicit none

  double precision :: zlanhs
  external :: zlanhs

  ! 3x3 upper Hessenberg matrix (stored col-major, LDA=3)
  complex*16 :: A(3,3)
  double precision :: A_real(18)
  equivalence (A, A_real)
  double precision :: work(3)
  double precision :: val

  ! 4x4 matrix
  complex*16 :: B(4,4)
  double precision :: B_real(32)
  equivalence (B, B_real)
  double precision :: work4(4)

  ! Test 1: Max norm ('M') of 3x3 upper Hessenberg
  ! Upper Hessenberg: nonzero on upper triangle + first subdiagonal
  ! A = [ (1+2i)   (3+1i)   (5+0i) ]
  !     [ (2+1i)   (4+3i)   (6+2i) ]
  !     [ (0+0i)   (1+1i)   (7+1i) ]
  A(1,1) = (1.0d0, 2.0d0)
  A(2,1) = (2.0d0, 1.0d0)
  A(3,1) = (0.0d0, 0.0d0)
  A(1,2) = (3.0d0, 1.0d0)
  A(2,2) = (4.0d0, 3.0d0)
  A(3,2) = (1.0d0, 1.0d0)
  A(1,3) = (5.0d0, 0.0d0)
  A(2,3) = (6.0d0, 2.0d0)
  A(3,3) = (7.0d0, 1.0d0)

  val = zlanhs('M', 3, A, 3, work)
  call begin_test('zlanhs_max_norm')
  call print_scalar('result', val)
  call end_test()

  ! Test 2: One norm ('1')
  val = zlanhs('1', 3, A, 3, work)
  call begin_test('zlanhs_one_norm')
  call print_scalar('result', val)
  call end_test()

  ! Test 3: One norm ('O')
  val = zlanhs('O', 3, A, 3, work)
  call begin_test('zlanhs_one_norm_o')
  call print_scalar('result', val)
  call end_test()

  ! Test 4: Infinity norm ('I')
  val = zlanhs('I', 3, A, 3, work)
  call begin_test('zlanhs_inf_norm')
  call print_scalar('result', val)
  call end_test()

  ! Test 5: Frobenius norm ('F')
  val = zlanhs('F', 3, A, 3, work)
  call begin_test('zlanhs_frob_norm')
  call print_scalar('result', val)
  call end_test()

  ! Test 6: Frobenius norm ('E') - synonym
  val = zlanhs('E', 3, A, 3, work)
  call begin_test('zlanhs_frob_norm_e')
  call print_scalar('result', val)
  call end_test()

  ! Test 7: N=0 (empty matrix)
  val = zlanhs('M', 0, A, 3, work)
  call begin_test('zlanhs_empty')
  call print_scalar('result', val)
  call end_test()

  ! Test 8: N=1 (1x1 matrix)
  A(1,1) = (3.0d0, 4.0d0)
  val = zlanhs('M', 1, A, 3, work)
  call begin_test('zlanhs_n1_max')
  call print_scalar('result', val)
  call end_test()

  val = zlanhs('1', 1, A, 3, work)
  call begin_test('zlanhs_n1_one')
  call print_scalar('result', val)
  call end_test()

  val = zlanhs('I', 1, A, 3, work)
  call begin_test('zlanhs_n1_inf')
  call print_scalar('result', val)
  call end_test()

  val = zlanhs('F', 1, A, 3, work)
  call begin_test('zlanhs_n1_frob')
  call print_scalar('result', val)
  call end_test()

  ! Test 9: 4x4 upper Hessenberg for broader coverage
  B(1,1) = (1.0d0, 0.5d0)
  B(2,1) = (0.3d0, 0.7d0)
  B(3,1) = (0.0d0, 0.0d0)
  B(4,1) = (0.0d0, 0.0d0)
  B(1,2) = (2.0d0, 1.0d0)
  B(2,2) = (1.5d0, 0.5d0)
  B(3,2) = (0.8d0, 0.2d0)
  B(4,2) = (0.0d0, 0.0d0)
  B(1,3) = (0.5d0, 0.3d0)
  B(2,3) = (3.0d0, 1.0d0)
  B(3,3) = (2.0d0, 0.0d0)
  B(4,3) = (1.0d0, 0.5d0)
  B(1,4) = (0.1d0, 0.2d0)
  B(2,4) = (0.4d0, 0.6d0)
  B(3,4) = (1.5d0, 1.5d0)
  B(4,4) = (4.0d0, 0.0d0)

  val = zlanhs('M', 4, B, 4, work4)
  call begin_test('zlanhs_4x4_max')
  call print_scalar('result', val)
  call end_test()

  val = zlanhs('1', 4, B, 4, work4)
  call begin_test('zlanhs_4x4_one')
  call print_scalar('result', val)
  call end_test()

  val = zlanhs('I', 4, B, 4, work4)
  call begin_test('zlanhs_4x4_inf')
  call print_scalar('result', val)
  call end_test()

  val = zlanhs('F', 4, B, 4, work4)
  call begin_test('zlanhs_4x4_frob')
  call print_scalar('result', val)
  call end_test()

end program
