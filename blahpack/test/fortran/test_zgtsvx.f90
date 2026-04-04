program test_zgtsvx
  use test_utils
  implicit none
  complex*16 :: dl(10), d(10), du(10)
  complex*16 :: dlf(10), df(10), duf(10), du2(10)
  double precision :: dl_r(20), d_r(20), du_r(20)
  double precision :: dlf_r(20), df_r(20), duf_r(20), du2_r(20)
  equivalence (dl, dl_r)
  equivalence (d, d_r)
  equivalence (du, du_r)
  equivalence (dlf, dlf_r)
  equivalence (df, df_r)
  equivalence (duf, duf_r)
  equivalence (du2, du2_r)
  integer :: ipiv(10)
  complex*16 :: b(10, 4), x(10, 4), work(100)
  double precision :: b_r(80), x_r(80)
  equivalence (b, b_r)
  equivalence (x, x_r)
  double precision :: rcond, ferr(4), berr(4), rwork(100)
  integer :: info

  ! Test 1: FACT='N', TRANS='N', basic 4x4, 1 RHS
  ! A tridiag: dl=[3+i, 1+2i, 2-i], d=[2+0.5i, 4+i, 5-0.5i, 6+2i], du=[-1+i, -2+0.5i, -3-i]
  dl(1) = (3.0d0, 1.0d0); dl(2) = (1.0d0, 2.0d0); dl(3) = (2.0d0, -1.0d0)
  d(1) = (2.0d0, 0.5d0); d(2) = (4.0d0, 1.0d0); d(3) = (5.0d0, -0.5d0)
  d(4) = (6.0d0, 2.0d0)
  du(1) = (-1.0d0, 1.0d0); du(2) = (-2.0d0, 0.5d0); du(3) = (-3.0d0, -1.0d0)
  ! b = A * [1; 1; 1; 1]
  ! row 1: d(1)+du(1) = (2+0.5i)+(-1+i) = (1, 1.5)
  ! row 2: dl(1)+d(2)+du(2) = (3+i)+(4+i)+(-2+0.5i) = (5, 2.5)
  ! row 3: dl(2)+d(3)+du(3) = (1+2i)+(5-0.5i)+(-3-i) = (3, 0.5)
  ! row 4: dl(3)+d(4) = (2-i)+(6+2i) = (8, 1)
  b(1, 1) = (1.0d0, 1.5d0)
  b(2, 1) = (5.0d0, 2.5d0)
  b(3, 1) = (3.0d0, 0.5d0)
  b(4, 1) = (8.0d0, 1.0d0)
  dlf = (0.0d0, 0.0d0); df = (0.0d0, 0.0d0)
  duf = (0.0d0, 0.0d0); du2 = (0.0d0, 0.0d0); ipiv = 0
  x = (0.0d0, 0.0d0)
  call zgtsvx('N', 'N', 4, 1, dl, d, du, dlf, df, duf, du2, ipiv, &
              b, 10, x, 10, rcond, ferr, berr, work, rwork, info)
  call begin_test('fact_n_trans_n')
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_array('x', x_r(1:8), 8)
  call print_array('ferr', ferr(1:1), 1)
  call print_array('berr', berr(1:1), 1)
  call print_array('dlf', dlf_r(1:6), 6)
  call print_array('df', df_r(1:8), 8)
  call print_array('duf', duf_r(1:6), 6)
  call print_array('du2', du2_r(1:4), 4)
  call print_int_array('ipiv', ipiv(1:4), 4)
  call end_test()

  ! Test 2: FACT='F' (already factored), TRANS='N', same system
  x = (0.0d0, 0.0d0)
  call zgtsvx('F', 'N', 4, 1, dl, d, du, dlf, df, duf, du2, ipiv, &
              b, 10, x, 10, rcond, ferr, berr, work, rwork, info)
  call begin_test('fact_f_trans_n')
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_array('x', x_r(1:8), 8)
  call print_array('ferr', ferr(1:1), 1)
  call print_array('berr', berr(1:1), 1)
  call end_test()

  ! Test 3: FACT='N', TRANS='T' (transpose), 4x4, 1 RHS
  ! b = A^T * [1; 1; 1; 1]
  ! A^T row 1: d(1), dl(1), 0, 0 => (2+0.5i)+(3+i) = (5,1.5)
  ! A^T row 2: du(1), d(2), dl(2), 0 => (-1+i)+(4+i)+(1+2i) = (4,4)
  ! A^T row 3: 0, du(2), d(3), dl(3) => (-2+0.5i)+(5-0.5i)+(2-i) = (5,-1)
  ! A^T row 4: 0, 0, du(3), d(4) => (-3-i)+(6+2i) = (3,1)
  b(1, 1) = (5.0d0, 1.5d0)
  b(2, 1) = (4.0d0, 4.0d0)
  b(3, 1) = (5.0d0, -1.0d0)
  b(4, 1) = (3.0d0, 1.0d0)
  dlf = (0.0d0, 0.0d0); df = (0.0d0, 0.0d0)
  duf = (0.0d0, 0.0d0); du2 = (0.0d0, 0.0d0); ipiv = 0
  x = (0.0d0, 0.0d0)
  call zgtsvx('N', 'T', 4, 1, dl, d, du, dlf, df, duf, du2, ipiv, &
              b, 10, x, 10, rcond, ferr, berr, work, rwork, info)
  call begin_test('fact_n_trans_t')
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_array('x', x_r(1:8), 8)
  call print_array('ferr', ferr(1:1), 1)
  call print_array('berr', berr(1:1), 1)
  call end_test()

  ! Test 4: FACT='N', TRANS='C' (conjugate transpose), 4x4, 1 RHS
  ! b = A^H * [1; 1; 1; 1]
  ! A^H row 1: conj(d(1)), conj(dl(1)), 0, 0 => (2-0.5i)+(3-i) = (5,-1.5)
  ! A^H row 2: conj(du(1)), conj(d(2)), conj(dl(2)), 0 => (-1-i)+(4-i)+(1-2i) = (4,-4)
  ! A^H row 3: 0, conj(du(2)), conj(d(3)), conj(dl(3)) => (-2-0.5i)+(5+0.5i)+(2+i) = (5,1)
  ! A^H row 4: 0, 0, conj(du(3)), conj(d(4)) => (-3+i)+(6-2i) = (3,-1)
  b(1, 1) = (5.0d0, -1.5d0)
  b(2, 1) = (4.0d0, -4.0d0)
  b(3, 1) = (5.0d0, 1.0d0)
  b(4, 1) = (3.0d0, -1.0d0)
  dlf = (0.0d0, 0.0d0); df = (0.0d0, 0.0d0)
  duf = (0.0d0, 0.0d0); du2 = (0.0d0, 0.0d0); ipiv = 0
  x = (0.0d0, 0.0d0)
  call zgtsvx('N', 'C', 4, 1, dl, d, du, dlf, df, duf, du2, ipiv, &
              b, 10, x, 10, rcond, ferr, berr, work, rwork, info)
  call begin_test('fact_n_trans_c')
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_array('x', x_r(1:8), 8)
  call print_array('ferr', ferr(1:1), 1)
  call print_array('berr', berr(1:1), 1)
  call end_test()

  ! Test 5: FACT='N', TRANS='N', multiple RHS (2)
  b(1, 1) = (1.0d0, 1.5d0)
  b(2, 1) = (5.0d0, 2.5d0)
  b(3, 1) = (3.0d0, 0.5d0)
  b(4, 1) = (8.0d0, 1.0d0)
  ! b(:,2) = A * [2+i; 3-i; 1+0.5i; 4-2i]
  ! row 1: d(1)*(2+i) + du(1)*(3-i) = (2+0.5i)*(2+i) + (-1+i)*(3-i)
  !       = (3.5, 3) + (-2, 4) = (1.5, 7)
  ! Compute actual product in Fortran:
  b(1, 2) = d(1)*(2.0d0, 1.0d0) + du(1)*(3.0d0, -1.0d0)
  b(2, 2) = dl(1)*(2.0d0, 1.0d0) + d(2)*(3.0d0, -1.0d0) + du(2)*(1.0d0, 0.5d0)
  b(3, 2) = dl(2)*(3.0d0, -1.0d0) + d(3)*(1.0d0, 0.5d0) + du(3)*(4.0d0, -2.0d0)
  b(4, 2) = dl(3)*(1.0d0, 0.5d0) + d(4)*(4.0d0, -2.0d0)
  dlf = (0.0d0, 0.0d0); df = (0.0d0, 0.0d0)
  duf = (0.0d0, 0.0d0); du2 = (0.0d0, 0.0d0); ipiv = 0
  x = (0.0d0, 0.0d0)
  call zgtsvx('N', 'N', 4, 2, dl, d, du, dlf, df, duf, du2, ipiv, &
              b, 10, x, 10, rcond, ferr, berr, work, rwork, info)
  call begin_test('multi_rhs')
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_array('x1', x_r(1:8), 8)
  call print_array('x2', x_r(21:28), 8)
  call print_array('ferr', ferr(1:2), 2)
  call print_array('berr', berr(1:2), 2)
  call end_test()

  ! Test 6: N=1
  d(1) = (5.0d0, 1.0d0)
  b(1, 1) = (10.0d0, 2.0d0)
  dlf = (0.0d0, 0.0d0); df = (0.0d0, 0.0d0)
  duf = (0.0d0, 0.0d0); du2 = (0.0d0, 0.0d0); ipiv = 0
  x = (0.0d0, 0.0d0)
  call zgtsvx('N', 'N', 1, 1, dl, d, du, dlf, df, duf, du2, ipiv, &
              b, 10, x, 10, rcond, ferr, berr, work, rwork, info)
  call begin_test('n_one')
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_array('x', x_r(1:2), 2)
  call print_array('ferr', ferr(1:1), 1)
  call print_array('berr', berr(1:1), 1)
  call end_test()

  ! Test 7: N=0 (quick return)
  call zgtsvx('N', 'N', 0, 1, dl, d, du, dlf, df, duf, du2, ipiv, &
              b, 10, x, 10, rcond, ferr, berr, work, rwork, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 8: singular matrix (d(1)=0)
  dl(1) = (0.0d0, 0.0d0); dl(2) = (0.0d0, 0.0d0)
  d(1) = (0.0d0, 0.0d0); d(2) = (2.0d0, 1.0d0); d(3) = (3.0d0, 0.0d0)
  du(1) = (1.0d0, 0.0d0); du(2) = (1.0d0, 0.5d0)
  b(1, 1) = (1.0d0, 0.0d0); b(2, 1) = (2.0d0, 1.0d0); b(3, 1) = (3.0d0, 0.0d0)
  dlf = (0.0d0, 0.0d0); df = (0.0d0, 0.0d0)
  duf = (0.0d0, 0.0d0); du2 = (0.0d0, 0.0d0); ipiv = 0
  x = (0.0d0, 0.0d0)
  call zgtsvx('N', 'N', 3, 1, dl, d, du, dlf, df, duf, du2, ipiv, &
              b, 10, x, 10, rcond, ferr, berr, work, rwork, info)
  call begin_test('singular')
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call end_test()

  ! Test 9: 5x5 with pivoting
  dl(1) = (5.0d0, 1.0d0); dl(2) = (7.0d0, -1.0d0); dl(3) = (9.0d0, 2.0d0)
  dl(4) = (2.0d0, 0.5d0)
  d(1) = (1.0d0, 0.0d0); d(2) = (3.0d0, 1.0d0); d(3) = (2.0d0, -1.0d0)
  d(4) = (1.0d0, 0.5d0); d(5) = (8.0d0, 0.0d0)
  du(1) = (2.0d0, -0.5d0); du(2) = (4.0d0, 1.0d0); du(3) = (6.0d0, 0.0d0)
  du(4) = (3.0d0, -1.0d0)
  ! b = A * [1;1;1;1;1]
  b(1, 1) = d(1) + du(1)
  b(2, 1) = dl(1) + d(2) + du(2)
  b(3, 1) = dl(2) + d(3) + du(3)
  b(4, 1) = dl(3) + d(4) + du(4)
  b(5, 1) = dl(4) + d(5)
  dlf = (0.0d0, 0.0d0); df = (0.0d0, 0.0d0)
  duf = (0.0d0, 0.0d0); du2 = (0.0d0, 0.0d0); ipiv = 0
  x = (0.0d0, 0.0d0)
  call zgtsvx('N', 'N', 5, 1, dl, d, du, dlf, df, duf, du2, ipiv, &
              b, 10, x, 10, rcond, ferr, berr, work, rwork, info)
  call begin_test('pivot_5x5')
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_array('x', x_r(1:10), 10)
  call print_array('ferr', ferr(1:1), 1)
  call print_array('berr', berr(1:1), 1)
  call end_test()

end program
