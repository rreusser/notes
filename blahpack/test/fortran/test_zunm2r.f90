program test_zunm2r
  use test_utils
  implicit none

  ! We will do QR of a 3x2 matrix A, then apply Q or Q^H to a 3x3 identity
  ! from the left and right, with trans='N' and trans='C'
  complex*16 :: A(3, 3), C(3, 3), TAU(3), WORK(100)
  double precision :: A_r(18), C_r(18), TAU_r(6)
  equivalence (A, A_r)
  equivalence (C, C_r)
  equivalence (TAU, TAU_r)
  integer :: info, i, j

  ! First, compute QR of a 3x2 matrix
  ! A = [1+0i 4+1i; 2+0i 5+1i; 3+0i 6+1i]
  ! Store in column-major
  A = (0.0d0, 0.0d0)
  A(1,1) = (1.0d0, 0.0d0); A(2,1) = (2.0d0, 0.0d0); A(3,1) = (3.0d0, 0.0d0)
  A(1,2) = (4.0d0, 1.0d0); A(2,2) = (5.0d0, 1.0d0); A(3,2) = (6.0d0, 1.0d0)
  TAU = (0.0d0, 0.0d0)
  call zgeqr2(3, 2, A, 3, TAU, WORK, info)

  ! Now A contains the QR factors, TAU contains the reflector scalars
  ! Q is 3x3 (K=2 reflectors)

  ! Test 1: Left, No transpose: C := Q * C where C = I_3
  C = (0.0d0, 0.0d0)
  C(1,1) = (1.0d0, 0.0d0); C(2,2) = (1.0d0, 0.0d0); C(3,3) = (1.0d0, 0.0d0)
  call zunm2r('L', 'N', 3, 3, 2, A, 3, TAU, C, 3, WORK, info)
  call begin_test('left_notrans')
  call print_int('info', info)
  call print_array('c', C_r, 18)
  call end_test()

  ! Test 2: Left, Conjugate transpose: C := Q^H * C where C = I_3
  C = (0.0d0, 0.0d0)
  C(1,1) = (1.0d0, 0.0d0); C(2,2) = (1.0d0, 0.0d0); C(3,3) = (1.0d0, 0.0d0)
  call zunm2r('L', 'C', 3, 3, 2, A, 3, TAU, C, 3, WORK, info)
  call begin_test('left_conjtrans')
  call print_int('info', info)
  call print_array('c', C_r, 18)
  call end_test()

  ! Test 3: Right, No transpose: C := C * Q where C = I_3
  C = (0.0d0, 0.0d0)
  C(1,1) = (1.0d0, 0.0d0); C(2,2) = (1.0d0, 0.0d0); C(3,3) = (1.0d0, 0.0d0)
  call zunm2r('R', 'N', 3, 3, 2, A, 3, TAU, C, 3, WORK, info)
  call begin_test('right_notrans')
  call print_int('info', info)
  call print_array('c', C_r, 18)
  call end_test()

  ! Test 4: Right, Conjugate transpose: C := C * Q^H where C = I_3
  C = (0.0d0, 0.0d0)
  C(1,1) = (1.0d0, 0.0d0); C(2,2) = (1.0d0, 0.0d0); C(3,3) = (1.0d0, 0.0d0)
  call zunm2r('R', 'C', 3, 3, 2, A, 3, TAU, C, 3, WORK, info)
  call begin_test('right_conjtrans')
  call print_int('info', info)
  call print_array('c', C_r, 18)
  call end_test()

  ! Test 5: M=0 quick return
  call zunm2r('L', 'N', 0, 3, 0, A, 1, TAU, C, 1, WORK, info)
  call begin_test('m_zero')
  call print_int('info', info)
  call end_test()

  ! Test 6: N=0 quick return
  call zunm2r('L', 'N', 3, 0, 0, A, 3, TAU, C, 3, WORK, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 7: K=0 quick return
  call zunm2r('L', 'N', 3, 3, 0, A, 3, TAU, C, 3, WORK, info)
  call begin_test('k_zero')
  call print_int('info', info)
  call end_test()

  ! Test 8: Apply to a non-identity matrix, left, notrans
  ! Use a 3x2 matrix C
  ! C = [1+1i 2-1i; 3+0i 0+2i; -1+1i 4+0i]
  A = (0.0d0, 0.0d0)
  A(1,1) = (1.0d0, 0.0d0); A(2,1) = (2.0d0, 0.0d0); A(3,1) = (3.0d0, 0.0d0)
  A(1,2) = (4.0d0, 1.0d0); A(2,2) = (5.0d0, 1.0d0); A(3,2) = (6.0d0, 1.0d0)
  TAU = (0.0d0, 0.0d0)
  call zgeqr2(3, 2, A, 3, TAU, WORK, info)

  C = (0.0d0, 0.0d0)
  C(1,1) = (1.0d0, 1.0d0); C(2,1) = (3.0d0, 0.0d0); C(3,1) = (-1.0d0, 1.0d0)
  C(1,2) = (2.0d0, -1.0d0); C(2,2) = (0.0d0, 2.0d0); C(3,2) = (4.0d0, 0.0d0)
  call zunm2r('L', 'N', 3, 2, 2, A, 3, TAU, C, 3, WORK, info)
  call begin_test('left_notrans_rect')
  call print_int('info', info)
  call print_array('c', C_r, 12)
  call end_test()

  ! Test 9: Right, notrans with rectangular C (2x3)
  ! Need QR of a 3x2 matrix A for right side (NQ = N = 3, so K <= 3)
  ! Actually for right side, NQ = N, so we need K <= N
  ! Let's do a 2x3 C with K=2 reflectors from a 3x2 QR
  A = (0.0d0, 0.0d0)
  A(1,1) = (1.0d0, 0.0d0); A(2,1) = (2.0d0, 0.0d0); A(3,1) = (3.0d0, 0.0d0)
  A(1,2) = (4.0d0, 1.0d0); A(2,2) = (5.0d0, 1.0d0); A(3,2) = (6.0d0, 1.0d0)
  TAU = (0.0d0, 0.0d0)
  call zgeqr2(3, 2, A, 3, TAU, WORK, info)

  ! For right multiplication, NQ = N = 3, K=2, K <= NQ, LDA >= NQ = 3
  C = (0.0d0, 0.0d0)
  C(1,1) = (1.0d0, 0.0d0); C(2,1) = (0.0d0, 1.0d0)
  C(1,2) = (2.0d0, 1.0d0); C(2,2) = (1.0d0, -1.0d0)
  C(1,3) = (-1.0d0, 0.0d0); C(2,3) = (3.0d0, 2.0d0)
  ! M=2, N=3, K=2, side='R' => NQ=N=3
  call zunm2r('R', 'N', 2, 3, 2, A, 3, TAU, C, 2, WORK, info)
  call begin_test('right_notrans_rect')
  call print_int('info', info)
  call print_array('c', C_r, 12)
  call end_test()

end program
