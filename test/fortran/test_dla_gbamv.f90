program test_dla_gbamv
  use test_utils
  implicit none
  integer, parameter :: BLAS_NO_TRANS = 111
  integer, parameter :: BLAS_TRANS = 112

  ! Band matrix storage workspace (LDAB rows, then columns).
  ! LDAB is fixed = 3 for all tests (max(kl+ku+1) = 3).
  double precision :: AB(3, 10)
  double precision :: x(20), y(20)
  integer :: m, n, kl, ku, ldab

  ! ==================================================================
  ! Test 1: no-transpose, KL=1 KU=1, 4x4 tridiagonal
  ! Full A:
  !   [  1  -2   0   0 ]
  !   [  3   4  -5   0 ]
  !   [  0  -6   7   8 ]
  !   [  0   0  -9  10 ]
  ! Band storage (KL=1, KU=1, LDAB=3):
  !   row (KU+1+i-j): AB(KU+1+i-j, j), 1-based
  !   col 1: AB(2,1)=1, AB(3,1)=3
  !   col 2: AB(1,2)=-2, AB(2,2)=4, AB(3,2)=-6
  !   col 3: AB(1,3)=-5, AB(2,3)=7, AB(3,3)=-9
  !   col 4: AB(1,4)=8, AB(2,4)=10
  m = 4; n = 4; kl = 1; ku = 1; ldab = 3
  AB = 0.0d0
  AB(2,1) =  1.0d0; AB(3,1) =  3.0d0
  AB(1,2) = -2.0d0; AB(2,2) =  4.0d0; AB(3,2) = -6.0d0
  AB(1,3) = -5.0d0; AB(2,3) =  7.0d0; AB(3,3) = -9.0d0
  AB(1,4) =  8.0d0; AB(2,4) = 10.0d0
  x(1) = 1.0d0; x(2) = -2.0d0; x(3) = 3.0d0; x(4) = -4.0d0
  y = 0.0d0
  call DLA_GBAMV(BLAS_NO_TRANS, m, n, kl, ku, 1.0d0, AB, ldab, x, 1, 0.0d0, y, 1)
  call begin_test('notrans_kl1_ku1')
  call print_array('y', y, m)
  call end_test()

  ! Test 2: transpose same matrix
  y = 0.0d0
  call DLA_GBAMV(BLAS_TRANS, m, n, kl, ku, 1.0d0, AB, ldab, x, 1, 0.0d0, y, 1)
  call begin_test('trans_kl1_ku1')
  call print_array('y', y, n)
  call end_test()

  ! Test 3: no-transpose with alpha=2, beta=0.5, nonzero y
  y(1) = -1.0d0; y(2) =  2.0d0; y(3) = -3.0d0; y(4) = 4.0d0
  call DLA_GBAMV(BLAS_NO_TRANS, m, n, kl, ku, 2.0d0, AB, ldab, x, 1, 0.5d0, y, 1)
  call begin_test('notrans_scaled')
  call print_array('y', y, m)
  call end_test()

  ! Test 4: transpose scaled
  y(1) = -1.0d0; y(2) =  2.0d0; y(3) = -3.0d0; y(4) = 4.0d0
  call DLA_GBAMV(BLAS_TRANS, m, n, kl, ku, 2.0d0, AB, ldab, x, 1, 0.5d0, y, 1)
  call begin_test('trans_scaled')
  call print_array('y', y, n)
  call end_test()

  ! ==================================================================
  ! Test 5: KL=2 KU=0 lower banded (no-trans), 4x4
  ! Full A:
  !   [  1   0   0   0 ]
  !   [  2   3   0   0 ]
  !   [  4   5   6   0 ]
  !   [  0   7   8   9 ]
  ! Band storage (KL=2, KU=0, LDAB=3):
  !   col 1: AB(1,1)=1, AB(2,1)=2, AB(3,1)=4
  !   col 2: AB(1,2)=3, AB(2,2)=5, AB(3,2)=7
  !   col 3: AB(1,3)=6, AB(2,3)=8
  !   col 4: AB(1,4)=9
  m = 4; n = 4; kl = 2; ku = 0; ldab = 3
  AB = 0.0d0
  AB(1,1) = 1.0d0; AB(2,1) = 2.0d0; AB(3,1) = 4.0d0
  AB(1,2) = 3.0d0; AB(2,2) = 5.0d0; AB(3,2) = 7.0d0
  AB(1,3) = 6.0d0; AB(2,3) = 8.0d0
  AB(1,4) = 9.0d0
  x(1) = 1.0d0; x(2) = -1.0d0; x(3) = 2.0d0; x(4) = -2.0d0
  y = 0.0d0
  call DLA_GBAMV(BLAS_NO_TRANS, m, n, kl, ku, 1.0d0, AB, ldab, x, 1, 0.0d0, y, 1)
  call begin_test('notrans_kl2_ku0')
  call print_array('y', y, m)
  call end_test()

  ! Test 6: same, transpose
  y = 0.0d0
  call DLA_GBAMV(BLAS_TRANS, m, n, kl, ku, 1.0d0, AB, ldab, x, 1, 0.0d0, y, 1)
  call begin_test('trans_kl2_ku0')
  call print_array('y', y, n)
  call end_test()

  ! ==================================================================
  ! Test 7: KL=0 KU=2 upper banded (no-trans), 4x4
  ! Full A:
  !   [  1   2   3   0 ]
  !   [  0   4   5   6 ]
  !   [  0   0   7   8 ]
  !   [  0   0   0   9 ]
  ! Band storage (KL=0, KU=2, LDAB=3):
  !   col 1: AB(3,1)=1
  !   col 2: AB(2,2)=2, AB(3,2)=4
  !   col 3: AB(1,3)=3, AB(2,3)=5, AB(3,3)=7
  !   col 4: AB(1,4)=6, AB(2,4)=8, AB(3,4)=9
  m = 4; n = 4; kl = 0; ku = 2; ldab = 3
  AB = 0.0d0
  AB(3,1) = 1.0d0
  AB(2,2) = 2.0d0; AB(3,2) = 4.0d0
  AB(1,3) = 3.0d0; AB(2,3) = 5.0d0; AB(3,3) = 7.0d0
  AB(1,4) = 6.0d0; AB(2,4) = 8.0d0; AB(3,4) = 9.0d0
  x(1) = 1.0d0; x(2) = -1.0d0; x(3) = 2.0d0; x(4) = -2.0d0
  y = 0.0d0
  call DLA_GBAMV(BLAS_NO_TRANS, m, n, kl, ku, 1.0d0, AB, ldab, x, 1, 0.0d0, y, 1)
  call begin_test('notrans_kl0_ku2')
  call print_array('y', y, m)
  call end_test()

  ! Test 8: same, transpose
  y = 0.0d0
  call DLA_GBAMV(BLAS_TRANS, m, n, kl, ku, 1.0d0, AB, ldab, x, 1, 0.0d0, y, 1)
  call begin_test('trans_kl0_ku2')
  call print_array('y', y, n)
  call end_test()

  ! ==================================================================
  ! Test 10: alpha=0, beta=1 quick return
  m = 4; n = 4; kl = 1; ku = 1; ldab = 3
  AB = 0.0d0
  AB(2,1) =  1.0d0; AB(3,1) =  3.0d0
  AB(1,2) = -2.0d0; AB(2,2) =  4.0d0; AB(3,2) = -6.0d0
  AB(1,3) = -5.0d0; AB(2,3) =  7.0d0; AB(3,3) = -9.0d0
  AB(1,4) =  8.0d0; AB(2,4) = 10.0d0
  y(1) = 7.0d0; y(2) = 8.0d0; y(3) = 9.0d0; y(4) = 10.0d0
  call DLA_GBAMV(BLAS_NO_TRANS, m, n, kl, ku, 0.0d0, AB, ldab, x, 1, 1.0d0, y, 1)
  call begin_test('alpha_zero_beta_one')
  call print_array('y', y, m)
  call end_test()

  ! Test 11: alpha=0, beta=2 (pure |y| * 2)
  y(1) = -1.0d0; y(2) = 2.0d0; y(3) = -3.0d0; y(4) = 4.0d0
  call DLA_GBAMV(BLAS_NO_TRANS, m, n, kl, ku, 0.0d0, AB, ldab, x, 1, 2.0d0, y, 1)
  call begin_test('alpha_zero_beta_two')
  call print_array('y', y, m)
  call end_test()

  ! Test 12: symbolic-zero path: row 1 of A is all zero (replace kl=1/ku=1
  ! tridiag with a zero first row except subdiag, and y(1)=0, so no
  ! perturbation on row 1)
  m = 4; n = 4; kl = 1; ku = 1; ldab = 3
  AB = 0.0d0
  ! row 1 = all zero: AB(2,1)=0 (diag), AB(1,2)=0 (superdiag)
  ! rows 2..4 populated:
  AB(3,1) = 3.0d0                     ! A(2,1)
  AB(2,2) = 4.0d0; AB(3,2) = -6.0d0   ! A(2,2), A(3,2)
  AB(1,3) = -5.0d0; AB(2,3) = 7.0d0; AB(3,3) = -9.0d0
  AB(1,4) = 8.0d0; AB(2,4) = 10.0d0
  x(1) = 1.0d0; x(2) = -2.0d0; x(3) = 3.0d0; x(4) = -4.0d0
  y(1) = 0.0d0; y(2) = 0.0d0; y(3) = 0.0d0; y(4) = 0.0d0
  call DLA_GBAMV(BLAS_NO_TRANS, m, n, kl, ku, 1.0d0, AB, ldab, x, 1, 0.0d0, y, 1)
  call begin_test('symbolic_zero_row')
  call print_array('y', y, m)
  call end_test()

  ! ==================================================================
  ! Test 13: non-unit incx path, KL=1 KU=1 no-trans
  m = 4; n = 4; kl = 1; ku = 1; ldab = 3
  AB = 0.0d0
  AB(2,1) =  1.0d0; AB(3,1) =  3.0d0
  AB(1,2) = -2.0d0; AB(2,2) =  4.0d0; AB(3,2) = -6.0d0
  AB(1,3) = -5.0d0; AB(2,3) =  7.0d0; AB(3,3) = -9.0d0
  AB(1,4) =  8.0d0; AB(2,4) = 10.0d0
  x = 0.0d0
  x(1) = 1.0d0; x(3) = -2.0d0; x(5) = 3.0d0; x(7) = -4.0d0
  y = 0.0d0
  call DLA_GBAMV(BLAS_NO_TRANS, m, n, kl, ku, 1.0d0, AB, ldab, x, 2, 0.0d0, y, 1)
  call begin_test('notrans_incx2')
  call print_array('y', y, m)
  call end_test()

  ! Test 14: negative incx/incy (trans)
  x(1) = 1.0d0; x(2) = -2.0d0; x(3) = 3.0d0; x(4) = -4.0d0
  y(1) = 1.0d0; y(2) = 2.0d0; y(3) = 3.0d0; y(4) = 4.0d0
  call DLA_GBAMV(BLAS_TRANS, m, n, kl, ku, 1.0d0, AB, ldab, x, -1, 1.0d0, y, -1)
  call begin_test('trans_negincx_negincy')
  call print_array('y', y, n)
  call end_test()

end program
