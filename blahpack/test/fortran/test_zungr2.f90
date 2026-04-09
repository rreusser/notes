program test_zungr2
  use test_utils
  implicit none

  integer :: info
  complex*16 :: A(16), TAU(4), WORK(4)
  double precision :: A_r(32), TAU_r(8)
  equivalence (A, A_r)
  equivalence (TAU, TAU_r)

  ! Test 1: M=0 quick return
  info = -99
  call zungr2(0, 3, 0, A, 1, TAU, WORK, info)
  call begin_test('zungr2_m0')
  call print_int('info', info)
  call end_test()

  ! Test 2: 3x3 identity (K=0)
  ! LDA=3, M=3, N=3, K=0
  A(1:9) = (0.0d0, 0.0d0)
  TAU = (0.0d0, 0.0d0)
  WORK = (0.0d0, 0.0d0)
  call zungr2(3, 3, 0, A, 3, TAU, WORK, info)
  call begin_test('zungr2_identity_k0')
  call print_int('info', info)
  call print_array('A', A_r, 2*3*3)
  call end_test()

  ! Test 3: 3x3, K=1, single reflector
  ! For zungr2, reflectors are stored in rows, last K rows
  ! Row M-K+1 = row 3 (1-based) stores the reflector vector
  ! The reflector vector extends along the row to column N-M+II = 3-3+3 = 3
  ! LDA=3
  A(1:9) = (0.0d0, 0.0d0)
  ! Row 3 (index 3,6,9): reflector in columns 1..3
  A(3) = (0.5d0, 0.25d0)    ! A(3,1)
  A(6) = (0.3d0, -0.1d0)    ! A(3,2)
  A(9) = (1.0d0, 0.0d0)     ! A(3,3) - diagonal, will be set to 1-conj(tau)
  TAU(1) = (1.2d0, 0.3d0)
  WORK = (0.0d0, 0.0d0)
  call zungr2(3, 3, 1, A, 3, TAU, WORK, info)
  call begin_test('zungr2_3x3_k1')
  call print_int('info', info)
  call print_array('A', A_r, 2*3*3)
  call end_test()

  ! Test 4: 3x3, K=2, two reflectors
  ! LDA=3
  A(1:9) = (0.0d0, 0.0d0)
  ! Row 2 (M-K+1=2): reflector 1, columns 1..N-M+II = 3-3+2 = 2
  A(2) = (0.4d0, 0.2d0)     ! A(2,1)
  A(5) = (1.0d0, 0.0d0)     ! A(2,2) - diagonal
  ! Row 3 (M-K+2=3): reflector 2, columns 1..N-M+II = 3-3+3 = 3
  A(3) = (0.6d0, 0.5d0)     ! A(3,1)
  A(6) = (0.1d0, -0.3d0)    ! A(3,2)
  A(9) = (1.0d0, 0.0d0)     ! A(3,3) - diagonal
  TAU(1) = (1.1d0, 0.2d0)
  TAU(2) = (0.9d0, -0.1d0)
  WORK = (0.0d0, 0.0d0)
  call zungr2(3, 3, 2, A, 3, TAU, WORK, info)
  call begin_test('zungr2_3x3_k2')
  call print_int('info', info)
  call print_array('A', A_r, 2*3*3)
  call end_test()

  ! Test 5: 3x4, K=2, rectangular (N > M)
  ! LDA=3
  A(1:12) = (0.0d0, 0.0d0)
  ! Row 2 (M-K+1=2): reflector 1, columns 1..N-M+II = 4-3+2 = 3
  A(2) = (0.3d0, 0.1d0)      ! A(2,1)
  A(5) = (0.2d0, -0.2d0)     ! A(2,2)
  A(8) = (1.0d0, 0.0d0)      ! A(2,3) - diagonal position N-M+II
  ! Row 3 (M-K+2=3): reflector 2, columns 1..N-M+II = 4-3+3 = 4
  A(3) = (0.1d0, 0.05d0)     ! A(3,1)
  A(6) = (-0.1d0, 0.2d0)     ! A(3,2)
  A(9) = (0.5d0, -0.1d0)     ! A(3,3)
  A(12) = (1.0d0, 0.0d0)     ! A(3,4) - diagonal position
  TAU(1) = (1.05d0, 0.1d0)
  TAU(2) = (0.8d0, 0.15d0)
  WORK = (0.0d0, 0.0d0)
  call zungr2(3, 4, 2, A, 3, TAU, WORK, info)
  call begin_test('zungr2_3x4_k2')
  call print_int('info', info)
  call print_array('A', A_r, 2*3*4)
  call end_test()

  ! Test 6: K=M (all reflectors), 3x3
  ! LDA=3
  A(1:9) = (0.0d0, 0.0d0)
  ! Row 1: reflector 1, N-M+II = 3-3+1 = 1 columns
  A(1) = (1.0d0, 0.0d0)     ! A(1,1) diagonal
  ! Row 2: reflector 2, N-M+II = 3-3+2 = 2 columns
  A(2) = (0.5d0, -0.2d0)    ! A(2,1)
  A(5) = (1.0d0, 0.0d0)     ! A(2,2) diagonal
  ! Row 3: reflector 3, N-M+II = 3-3+3 = 3 columns
  A(3) = (-0.1d0, 0.4d0)    ! A(3,1)
  A(6) = (0.3d0, 0.1d0)     ! A(3,2)
  A(9) = (1.0d0, 0.0d0)     ! A(3,3) diagonal
  TAU(1) = (1.3d0, -0.1d0)
  TAU(2) = (0.7d0, 0.4d0)
  TAU(3) = (1.1d0, 0.0d0)
  WORK = (0.0d0, 0.0d0)
  call zungr2(3, 3, 3, A, 3, TAU, WORK, info)
  call begin_test('zungr2_3x3_k3')
  call print_int('info', info)
  call print_array('A', A_r, 2*3*3)
  call end_test()

  ! Test 7: 1x1, K=1
  A(1) = (1.0d0, 0.0d0)
  TAU(1) = (0.5d0, 0.5d0)
  WORK = (0.0d0, 0.0d0)
  call zungr2(1, 1, 1, A, 1, TAU, WORK, info)
  call begin_test('zungr2_1x1_k1')
  call print_int('info', info)
  call print_array('A', A_r, 2)
  call end_test()

  ! Test 8: Use actual RQ factorization output
  ! Factor a 3x4 matrix first, then regenerate Q
  ! LDA=3
  A(1:12) = (0.0d0, 0.0d0)
  A(1)  = (1.0d0, 2.0d0)
  A(2)  = (3.0d0, 4.0d0)
  A(3)  = (5.0d0, 6.0d0)
  A(4)  = (2.0d0, -1.0d0)
  A(5)  = (1.0d0, 3.0d0)
  A(6)  = (4.0d0, -2.0d0)
  A(7)  = (1.0d0, 1.0d0)
  A(8)  = (-1.0d0, 2.0d0)
  A(9)  = (3.0d0, 0.0d0)
  A(10) = (0.0d0, 3.0d0)
  A(11) = (2.0d0, -1.0d0)
  A(12) = (-1.0d0, 4.0d0)
  call zgerqf(3, 4, A, 3, TAU, WORK, 4, info)
  call zungr2(3, 4, 3, A, 3, TAU, WORK, info)
  call begin_test('zungr2_from_rq')
  call print_int('info', info)
  call print_array('A', A_r, 2*3*4)
  call end_test()

end program
