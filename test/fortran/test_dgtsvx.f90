program test_dgtsvx
  use test_utils
  implicit none
  double precision :: dl(10), d(10), du(10)
  double precision :: dlf(10), df(10), duf(10), du2(10)
  integer :: ipiv(10)
  double precision :: b(10, 4), x(10, 4)
  double precision :: rcond, ferr(4), berr(4)
  double precision :: work(40)
  integer :: iwork(10), info

  ! Test 1: FACT='N', TRANS='N', basic 4x4, 1 RHS
  dl(1) = 3.0d0; dl(2) = 1.0d0; dl(3) = 2.0d0
  d(1) = 2.0d0; d(2) = 4.0d0; d(3) = 5.0d0; d(4) = 6.0d0
  du(1) = -1.0d0; du(2) = -2.0d0; du(3) = -3.0d0
  b(1, 1) = 0.0d0; b(2, 1) = 5.0d0; b(3, 1) = 5.0d0; b(4, 1) = 30.0d0
  dlf = 0.0d0; df = 0.0d0; duf = 0.0d0; du2 = 0.0d0; ipiv = 0
  x = 0.0d0
  call dgtsvx('N', 'N', 4, 1, dl, d, du, dlf, df, duf, du2, ipiv, &
              b, 10, x, 10, rcond, ferr, berr, work, iwork, info)
  call begin_test('fact_n_trans_n')
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_array('x', x(1:4, 1), 4)
  call print_array('ferr', ferr(1:1), 1)
  call print_array('berr', berr(1:1), 1)
  call print_array('dlf', dlf(1:3), 3)
  call print_array('df', df(1:4), 4)
  call print_array('duf', duf(1:3), 3)
  call print_array('du2', du2(1:2), 2)
  call print_int_array('ipiv', ipiv(1:4), 4)
  call end_test()

  ! Test 2: FACT='F' (already factored), TRANS='N', same system
  ! Use the factored arrays from test 1
  x = 0.0d0
  call dgtsvx('F', 'N', 4, 1, dl, d, du, dlf, df, duf, du2, ipiv, &
              b, 10, x, 10, rcond, ferr, berr, work, iwork, info)
  call begin_test('fact_f_trans_n')
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_array('x', x(1:4, 1), 4)
  call print_array('ferr', ferr(1:1), 1)
  call print_array('berr', berr(1:1), 1)
  call end_test()

  ! Test 3: FACT='N', TRANS='T', 4x4, 1 RHS
  ! b = A^T * [1,2,3,4]
  b(1, 1) = 8.0d0; b(2, 1) = 10.0d0; b(3, 1) = 19.0d0; b(4, 1) = 15.0d0
  dlf = 0.0d0; df = 0.0d0; duf = 0.0d0; du2 = 0.0d0; ipiv = 0
  x = 0.0d0
  call dgtsvx('N', 'T', 4, 1, dl, d, du, dlf, df, duf, du2, ipiv, &
              b, 10, x, 10, rcond, ferr, berr, work, iwork, info)
  call begin_test('fact_n_trans_t')
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_array('x', x(1:4, 1), 4)
  call print_array('ferr', ferr(1:1), 1)
  call print_array('berr', berr(1:1), 1)
  call end_test()

  ! Test 4: FACT='N', TRANS='N', multiple RHS
  b(1, 1) = 0.0d0; b(2, 1) = 5.0d0; b(3, 1) = 5.0d0; b(4, 1) = 30.0d0
  b(1, 2) = 4.0d0; b(2, 2) = 4.0d0; b(3, 2) = -4.0d0; b(4, 2) = 20.0d0
  dlf = 0.0d0; df = 0.0d0; duf = 0.0d0; du2 = 0.0d0; ipiv = 0
  x = 0.0d0
  call dgtsvx('N', 'N', 4, 2, dl, d, du, dlf, df, duf, du2, ipiv, &
              b, 10, x, 10, rcond, ferr, berr, work, iwork, info)
  call begin_test('multi_rhs')
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_array('x1', x(1:4, 1), 4)
  call print_array('x2', x(1:4, 2), 4)
  call print_array('ferr', ferr(1:2), 2)
  call print_array('berr', berr(1:2), 2)
  call end_test()

  ! Test 5: N=1
  d(1) = 5.0d0
  b(1, 1) = 10.0d0
  dlf = 0.0d0; df = 0.0d0; duf = 0.0d0; du2 = 0.0d0; ipiv = 0
  x = 0.0d0
  call dgtsvx('N', 'N', 1, 1, dl, d, du, dlf, df, duf, du2, ipiv, &
              b, 10, x, 10, rcond, ferr, berr, work, iwork, info)
  call begin_test('n_one')
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_array('x', x(1:1, 1), 1)
  call end_test()

  ! Test 6: N=0 (quick return)
  call dgtsvx('N', 'N', 0, 1, dl, d, du, dlf, df, duf, du2, ipiv, &
              b, 10, x, 10, rcond, ferr, berr, work, iwork, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 7: singular matrix (d(1)=0)
  dl(1) = 0.0d0; dl(2) = 0.0d0
  d(1) = 0.0d0; d(2) = 2.0d0; d(3) = 3.0d0
  du(1) = 1.0d0; du(2) = 1.0d0
  b(1, 1) = 1.0d0; b(2, 1) = 2.0d0; b(3, 1) = 3.0d0
  dlf = 0.0d0; df = 0.0d0; duf = 0.0d0; du2 = 0.0d0; ipiv = 0
  x = 0.0d0
  call dgtsvx('N', 'N', 3, 1, dl, d, du, dlf, df, duf, du2, ipiv, &
              b, 10, x, 10, rcond, ferr, berr, work, iwork, info)
  call begin_test('singular')
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call end_test()

  ! Test 8: 5x5 with pivoting
  dl(1) = 5.0d0; dl(2) = 7.0d0; dl(3) = 9.0d0; dl(4) = 2.0d0
  d(1) = 1.0d0; d(2) = 3.0d0; d(3) = 2.0d0; d(4) = 1.0d0; d(5) = 8.0d0
  du(1) = 2.0d0; du(2) = 4.0d0; du(3) = 6.0d0; du(4) = 3.0d0
  b(1, 1) = 3.0d0; b(2, 1) = 12.0d0; b(3, 1) = 15.0d0
  b(4, 1) = 13.0d0; b(5, 1) = 10.0d0
  dlf = 0.0d0; df = 0.0d0; duf = 0.0d0; du2 = 0.0d0; ipiv = 0
  x = 0.0d0
  call dgtsvx('N', 'N', 5, 1, dl, d, du, dlf, df, duf, du2, ipiv, &
              b, 10, x, 10, rcond, ferr, berr, work, iwork, info)
  call begin_test('pivot_5x5')
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_array('x', x(1:5, 1), 5)
  call print_array('ferr', ferr(1:1), 1)
  call print_array('berr', berr(1:1), 1)
  call end_test()

  ! Test 9: FACT='N', TRANS='C' (conjugate transpose = transpose for real)
  dl(1) = 3.0d0; dl(2) = 1.0d0; dl(3) = 2.0d0
  d(1) = 2.0d0; d(2) = 4.0d0; d(3) = 5.0d0; d(4) = 6.0d0
  du(1) = -1.0d0; du(2) = -2.0d0; du(3) = -3.0d0
  b(1, 1) = 8.0d0; b(2, 1) = 10.0d0; b(3, 1) = 19.0d0; b(4, 1) = 15.0d0
  dlf = 0.0d0; df = 0.0d0; duf = 0.0d0; du2 = 0.0d0; ipiv = 0
  x = 0.0d0
  call dgtsvx('N', 'C', 4, 1, dl, d, du, dlf, df, duf, du2, ipiv, &
              b, 10, x, 10, rcond, ferr, berr, work, iwork, info)
  call begin_test('fact_n_trans_c')
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_array('x', x(1:4, 1), 4)
  call end_test()

end program
