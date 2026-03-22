program test_zlarfb
  use test_utils
  implicit none

  ! We test the most common case: side='L', trans='C', direct='F', storev='C'
  ! This is what QR factorization uses.

  complex*16 :: V(25), T(9), C(25), work(25)
  double precision :: C_real(50), T_real(18), V_real(50)
  equivalence (V, V_real)
  equivalence (T, T_real)
  equivalence (C, C_real)
  integer :: i

  ! Test 1: side='L', trans='N', direct='F', storev='C'
  ! M=4, N=3, K=2
  ! V is 4x2 (unit lower triangular)
  V = (0.0d0, 0.0d0)
  V(1) = (1.0d0, 0.0d0)
  V(2) = (0.3d0, 0.2d0)
  V(3) = (-0.5d0, 0.1d0)
  V(4) = (0.4d0, -0.3d0)
  V(5) = (0.0d0, 0.0d0)
  V(6) = (1.0d0, 0.0d0)
  V(7) = (0.6d0, -0.4d0)
  V(8) = (-0.2d0, 0.5d0)

  ! T is 2x2 upper triangular (from zlarft)
  T = (0.0d0, 0.0d0)
  T(1) = (1.2d0, -0.3d0)  ! T(1,1)
  T(2) = (0.0d0, 0.0d0)   ! T(2,1)
  T(3) = (0.0d0, 0.0d0)   ! unused
  T(4) = (0.52d0, -0.15d0) ! T(1,2)
  T(5) = (1.5d0, 0.4d0)    ! T(2,2)
  T(6) = (0.0d0, 0.0d0)    ! unused

  ! C is 4x3
  C(1)  = (1.0d0, 0.0d0)
  C(2)  = (0.0d0, 1.0d0)
  C(3)  = (2.0d0, -1.0d0)
  C(4)  = (3.0d0, 0.5d0)
  C(5)  = (-1.0d0, 2.0d0)
  C(6)  = (0.5d0, 0.5d0)
  C(7)  = (1.5d0, -0.5d0)
  C(8)  = (-2.0d0, 1.0d0)
  C(9)  = (0.0d0, 0.0d0)
  C(10) = (1.0d0, 1.0d0)
  C(11) = (-0.5d0, 0.0d0)
  C(12) = (2.0d0, -2.0d0)

  work = (0.0d0, 0.0d0)
  call zlarfb('L', 'N', 'F', 'C', 4, 3, 2, V, 4, T, 3, C, 4, work, 3)
  call begin_test('zlarfb_left_notrans_fwd_col')
  call print_array('C', C_real, 24)
  call end_test()

  ! Test 2: side='L', trans='C', direct='F', storev='C'
  ! Same V and T; reset C
  C(1)  = (1.0d0, 0.0d0)
  C(2)  = (0.0d0, 1.0d0)
  C(3)  = (2.0d0, -1.0d0)
  C(4)  = (3.0d0, 0.5d0)
  C(5)  = (-1.0d0, 2.0d0)
  C(6)  = (0.5d0, 0.5d0)
  C(7)  = (1.5d0, -0.5d0)
  C(8)  = (-2.0d0, 1.0d0)
  C(9)  = (0.0d0, 0.0d0)
  C(10) = (1.0d0, 1.0d0)
  C(11) = (-0.5d0, 0.0d0)
  C(12) = (2.0d0, -2.0d0)

  work = (0.0d0, 0.0d0)
  call zlarfb('L', 'C', 'F', 'C', 4, 3, 2, V, 4, T, 3, C, 4, work, 3)
  call begin_test('zlarfb_left_conjtrans_fwd_col')
  call print_array('C', C_real, 24)
  call end_test()

  ! Test 3: side='R', trans='N', direct='F', storev='C'
  ! M=3, N=4, K=2
  ! V is 4x2 (same as above)
  ! C is 3x4
  C(1)  = (1.0d0, 0.0d0)
  C(2)  = (2.0d0, 1.0d0)
  C(3)  = (3.0d0, -1.0d0)
  C(4)  = (0.0d0, 1.0d0)
  C(5)  = (0.5d0, 0.5d0)
  C(6)  = (-1.0d0, 2.0d0)
  C(7)  = (1.5d0, -0.5d0)
  C(8)  = (-2.0d0, 1.0d0)
  C(9)  = (0.0d0, 0.0d0)
  C(10) = (1.0d0, 1.0d0)
  C(11) = (-0.5d0, 0.0d0)
  C(12) = (2.0d0, -2.0d0)

  work = (0.0d0, 0.0d0)
  call zlarfb('R', 'N', 'F', 'C', 3, 4, 2, V, 4, T, 3, C, 3, work, 3)
  call begin_test('zlarfb_right_notrans_fwd_col')
  call print_array('C', C_real, 24)
  call end_test()

  ! Test 4: M=0 or N=0 (quick return)
  C(1) = (1.0d0, 0.0d0)
  call zlarfb('L', 'N', 'F', 'C', 0, 3, 2, V, 4, T, 3, C, 1, work, 3)
  call begin_test('zlarfb_m_zero')
  call print_array('C', C_real, 2)
  call end_test()

  ! Test 5: Backward, Columnwise, Left, No transpose
  ! V for backward: last K rows have unit triangular
  V = (0.0d0, 0.0d0)
  V(1) = (0.3d0, 0.2d0)
  V(2) = (-0.5d0, 0.1d0)
  V(3) = (1.0d0, 0.0d0)
  V(4) = (0.0d0, 0.0d0)
  V(5) = (0.6d0, -0.4d0)
  V(6) = (-0.2d0, 0.5d0)
  V(7) = (0.4d0, -0.3d0)
  V(8) = (1.0d0, 0.0d0)

  ! T for backward is lower triangular
  T = (0.0d0, 0.0d0)
  T(1) = (1.2d0, -0.3d0)
  T(2) = (-1.22d0, -1.50d0)
  T(3) = (0.0d0, 0.0d0)
  T(4) = (0.0d0, 0.0d0)
  T(5) = (1.5d0, 0.4d0)
  T(6) = (0.0d0, 0.0d0)

  C(1)  = (1.0d0, 0.0d0)
  C(2)  = (0.0d0, 1.0d0)
  C(3)  = (2.0d0, -1.0d0)
  C(4)  = (3.0d0, 0.5d0)
  C(5)  = (-1.0d0, 2.0d0)
  C(6)  = (0.5d0, 0.5d0)
  C(7)  = (1.5d0, -0.5d0)
  C(8)  = (-2.0d0, 1.0d0)
  C(9)  = (0.0d0, 0.0d0)
  C(10) = (1.0d0, 1.0d0)
  C(11) = (-0.5d0, 0.0d0)
  C(12) = (2.0d0, -2.0d0)

  work = (0.0d0, 0.0d0)
  call zlarfb('L', 'N', 'B', 'C', 4, 3, 2, V, 4, T, 3, C, 4, work, 3)
  call begin_test('zlarfb_left_notrans_bwd_col')
  call print_array('C', C_real, 24)
  call end_test()

end program
