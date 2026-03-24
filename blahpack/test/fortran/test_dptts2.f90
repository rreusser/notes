program test_dptts2
  use test_utils
  implicit none
  double precision :: d(10), e(10), b(100)
  integer :: i

  ! ---------------------------------------------------------------
  ! Test 1: Basic 5x5, single RHS
  ! Factor: D = [4, 3, 2, 3, 4], E = [0.5, -0.5, 0.25, -0.25]
  ! (These are the D and E from an LDL^T factorization.)
  ! We construct b = L*D*L^T * x for a known x = [1, 2, 3, 4, 5].
  !
  ! L is unit lower bidiagonal with subdiag = E:
  !   L = [1        ;
  !        0.5  1   ;
  !        0  -0.5 1;
  !        ...      ]
  ! A = L*D*L^T is tridiagonal. We compute b = A*x.
  !
  ! Actually, to get b, compute L^T*x first, then D*(L^T*x), then L*(D*L^T*x).
  ! Or more directly: A has diagonal a_ii = d_i + e_{i-1}^2*d_{i-1} (for i>1)
  ! and off-diagonal a_{i,i+1} = e_i * d_i.
  !
  ! Let's just do forward computation: set x = [1,2,3,4,5], compute b = A*x
  ! by first applying L^T to x, then D, then L.
  !
  ! Easier: just call dptts2 and verify it inverts correctly.
  ! Set b to known values, call dptts2, check output.
  ! We'll use a simpler approach: set D, E, compute A, set x, compute b=A*x,
  ! then verify dptts2 recovers x.

  ! Tridiagonal matrix from LDL^T:
  ! diagonal of A: a_ii = d_i + e_{i-1}^2 * d_{i-1} for i>1, a_11 = d_1
  ! off-diag of A: a_{i,i+1} = e_i * d_i
  !
  ! D = [4, 3, 2, 3, 4], E = [0.5, -0.5, 0.25, -0.25]
  ! a_11 = 4
  ! a_22 = 3 + 0.5^2 * 4 = 3 + 1 = 4
  ! a_33 = 2 + (-0.5)^2 * 3 = 2 + 0.75 = 2.75
  ! a_44 = 3 + 0.25^2 * 2 = 3 + 0.125 = 3.125
  ! a_55 = 4 + (-0.25)^2 * 3 = 4 + 0.1875 = 4.1875
  ! off-diag: e_i*d_i = [2, -1.5, 0.5, -0.75]
  !
  ! x = [1, 2, 3, 4, 5]
  ! b_1 = 4*1 + 2*2 = 8
  ! b_2 = 2*1 + 4*2 + (-1.5)*3 = 2 + 8 - 4.5 = 5.5
  ! b_3 = (-1.5)*2 + 2.75*3 + 0.5*4 = -3 + 8.25 + 2 = 7.25
  ! b_4 = 0.5*3 + 3.125*4 + (-0.75)*5 = 1.5 + 12.5 - 3.75 = 10.25
  ! b_5 = (-0.75)*4 + 4.1875*5 = -3 + 20.9375 = 17.9375

  d(1) = 4.0d0; d(2) = 3.0d0; d(3) = 2.0d0; d(4) = 3.0d0; d(5) = 4.0d0
  e(1) = 0.5d0; e(2) = -0.5d0; e(3) = 0.25d0; e(4) = -0.25d0

  ! b stored col-major, LDB=5, NRHS=1
  b(1) = 8.0d0
  b(2) = 5.5d0
  b(3) = 7.25d0
  b(4) = 10.25d0
  b(5) = 17.9375d0

  call dptts2(5, 1, d, e, b, 5)

  call begin_test('basic_5x5_single_rhs')
  call print_array('x', b, 5)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 2: Multiple RHS (NRHS=2) with same 5x5 system
  ! x1 = [1, 2, 3, 4, 5], x2 = [5, 4, 3, 2, 1]
  ! b2_1 = 4*5 + 2*4 = 28
  ! b2_2 = 2*5 + 4*4 + (-1.5)*3 = 10 + 16 - 4.5 = 21.5
  ! b2_3 = (-1.5)*4 + 2.75*3 + 0.5*2 = -6 + 8.25 + 1 = 3.25
  ! b2_4 = 0.5*3 + 3.125*2 + (-0.75)*1 = 1.5 + 6.25 - 0.75 = 7.0
  ! b2_5 = (-0.75)*2 + 4.1875*1 = -1.5 + 4.1875 = 2.6875

  d(1) = 4.0d0; d(2) = 3.0d0; d(3) = 2.0d0; d(4) = 3.0d0; d(5) = 4.0d0
  e(1) = 0.5d0; e(2) = -0.5d0; e(3) = 0.25d0; e(4) = -0.25d0

  ! Column 1 of B (indices 1-5)
  b(1) = 8.0d0
  b(2) = 5.5d0
  b(3) = 7.25d0
  b(4) = 10.25d0
  b(5) = 17.9375d0
  ! Column 2 of B (indices 6-10)
  b(6) = 28.0d0
  b(7) = 21.5d0
  b(8) = 3.25d0
  b(9) = 7.0d0
  b(10) = 2.6875d0

  call dptts2(5, 2, d, e, b, 5)

  call begin_test('multi_rhs')
  call print_array('x', b, 10)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 3: N=1
  ! D = [3], no E, b = [9], expect x = 3
  d(1) = 3.0d0
  b(1) = 9.0d0

  call dptts2(1, 1, d, e, b, 1)

  call begin_test('n_eq_1')
  call print_array('x', b, 1)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 4: N=1 with NRHS=2
  d(1) = 4.0d0
  b(1) = 8.0d0
  b(2) = 12.0d0

  call dptts2(1, 2, d, e, b, 1)

  call begin_test('n_eq_1_multi_rhs')
  call print_array('x', b, 2)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 5: N=0 (quick return)
  ! After call, b should be unchanged
  b(1) = 42.0d0

  call dptts2(0, 1, d, e, b, 1)

  call begin_test('n_eq_0')
  call print_array('x', b, 1)
  call end_test()

end program
