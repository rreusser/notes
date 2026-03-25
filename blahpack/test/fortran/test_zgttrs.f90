program test_zgttrs
  use test_utils
  implicit none
  double precision :: dl_r(20), d_r(20), du_r(20), du2_r(20), b_r(80), packed_r(80)
  complex*16 :: dl(10), d(10), du(10), du2(10), b(10, 4), packed(40)
  equivalence (dl, dl_r)
  equivalence (d, d_r)
  equivalence (du, du_r)
  equivalence (du2, du2_r)
  equivalence (b, b_r)
  equivalence (packed, packed_r)
  integer :: ipiv(10), info, n, nrhs, ii, jj

  ! ============================================================
  ! Test 1: 5x5 no-transpose, single RHS
  n = 5; nrhs = 1
  dl(1) = (-1.0d0, 0.0d0); dl(2) = (-1.0d0, 0.0d0)
  dl(3) = (-1.0d0, 0.0d0); dl(4) = (-1.0d0, 0.0d0)
  d(1) = (2.0d0, 1.0d0); d(2) = (2.0d0, 1.0d0)
  d(3) = (2.0d0, 1.0d0); d(4) = (2.0d0, 1.0d0); d(5) = (2.0d0, 1.0d0)
  du(1) = (-1.0d0, 0.0d0); du(2) = (-1.0d0, 0.0d0)
  du(3) = (-1.0d0, 0.0d0); du(4) = (-1.0d0, 0.0d0)
  b(1,1) = (1.0d0, 0.0d0); b(2,1) = (0.0d0, 0.0d0)
  b(3,1) = (0.0d0, 0.0d0); b(4,1) = (0.0d0, 0.0d0); b(5,1) = (1.0d0, 0.0d0)

  call ZGTTRF(n, dl, d, du, du2, ipiv, info)
  call ZGTTRS('N', n, nrhs, dl, d, du, du2, ipiv, b, 10, info)
  do jj = 1, nrhs
    do ii = 1, n
      packed((jj-1)*n + ii) = b(ii, jj)
    end do
  end do
  call begin_test('notrans_single_rhs')
  call print_array('B', packed_r, 2*n*nrhs)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 2: 5x5 transpose
  dl(1) = (-1.0d0, 0.0d0); dl(2) = (-1.0d0, 0.0d0)
  dl(3) = (-1.0d0, 0.0d0); dl(4) = (-1.0d0, 0.0d0)
  d(1) = (2.0d0, 1.0d0); d(2) = (2.0d0, 1.0d0)
  d(3) = (2.0d0, 1.0d0); d(4) = (2.0d0, 1.0d0); d(5) = (2.0d0, 1.0d0)
  du(1) = (-1.0d0, 0.0d0); du(2) = (-1.0d0, 0.0d0)
  du(3) = (-1.0d0, 0.0d0); du(4) = (-1.0d0, 0.0d0)
  b(1,1) = (1.0d0, 0.0d0); b(2,1) = (0.0d0, 0.0d0)
  b(3,1) = (0.0d0, 0.0d0); b(4,1) = (0.0d0, 0.0d0); b(5,1) = (1.0d0, 0.0d0)

  call ZGTTRF(n, dl, d, du, du2, ipiv, info)
  call ZGTTRS('T', n, nrhs, dl, d, du, du2, ipiv, b, 10, info)
  do jj = 1, nrhs
    do ii = 1, n
      packed((jj-1)*n + ii) = b(ii, jj)
    end do
  end do
  call begin_test('trans_single_rhs')
  call print_array('B', packed_r, 2*n*nrhs)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 3: 5x5 conjugate transpose
  dl(1) = (-1.0d0, 0.0d0); dl(2) = (-1.0d0, 0.0d0)
  dl(3) = (-1.0d0, 0.0d0); dl(4) = (-1.0d0, 0.0d0)
  d(1) = (2.0d0, 1.0d0); d(2) = (2.0d0, 1.0d0)
  d(3) = (2.0d0, 1.0d0); d(4) = (2.0d0, 1.0d0); d(5) = (2.0d0, 1.0d0)
  du(1) = (-1.0d0, 0.0d0); du(2) = (-1.0d0, 0.0d0)
  du(3) = (-1.0d0, 0.0d0); du(4) = (-1.0d0, 0.0d0)
  b(1,1) = (1.0d0, 0.0d0); b(2,1) = (0.0d0, 0.0d0)
  b(3,1) = (0.0d0, 0.0d0); b(4,1) = (0.0d0, 0.0d0); b(5,1) = (1.0d0, 0.0d0)

  call ZGTTRF(n, dl, d, du, du2, ipiv, info)
  call ZGTTRS('C', n, nrhs, dl, d, du, du2, ipiv, b, 10, info)
  do jj = 1, nrhs
    do ii = 1, n
      packed((jj-1)*n + ii) = b(ii, jj)
    end do
  end do
  call begin_test('conjtrans_single_rhs')
  call print_array('B', packed_r, 2*n*nrhs)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 4: N=1
  n = 1; nrhs = 1
  d(1) = (5.0d0, 2.0d0)
  b(1,1) = (10.0d0, 4.0d0)
  call ZGTTRF(n, dl, d, du, du2, ipiv, info)
  call ZGTTRS('N', n, nrhs, dl, d, du, du2, ipiv, b, 10, info)
  packed(1) = b(1,1)
  call begin_test('n_one')
  call print_array('B', packed_r, 2)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 5: N=0
  n = 0; nrhs = 1
  call ZGTTRS('N', n, nrhs, dl, d, du, du2, ipiv, b, 1, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 6: 5x5 with pivoting forced, multiple RHS
  n = 5; nrhs = 2
  dl(1) = (10.0d0, 1.0d0); dl(2) = (10.0d0, 1.0d0)
  dl(3) = (10.0d0, 1.0d0); dl(4) = (10.0d0, 1.0d0)
  d(1) = (1.0d0, 0.0d0); d(2) = (1.0d0, 0.0d0)
  d(3) = (1.0d0, 0.0d0); d(4) = (1.0d0, 0.0d0); d(5) = (1.0d0, 0.0d0)
  du(1) = (2.0d0, 0.5d0); du(2) = (2.0d0, 0.5d0)
  du(3) = (2.0d0, 0.5d0); du(4) = (2.0d0, 0.5d0)
  b(1,1) = (3.0d0, 1.0d0); b(2,1) = (13.0d0, 2.0d0)
  b(3,1) = (13.0d0, 2.0d0); b(4,1) = (13.0d0, 2.0d0); b(5,1) = (11.0d0, 1.0d0)
  b(1,2) = (1.0d0, 0.0d0); b(2,2) = (1.0d0, 0.0d0)
  b(3,2) = (1.0d0, 0.0d0); b(4,2) = (1.0d0, 0.0d0); b(5,2) = (1.0d0, 0.0d0)

  call ZGTTRF(n, dl, d, du, du2, ipiv, info)
  call ZGTTRS('N', n, nrhs, dl, d, du, du2, ipiv, b, 10, info)
  do jj = 1, nrhs
    do ii = 1, n
      packed((jj-1)*n + ii) = b(ii, jj)
    end do
  end do
  call begin_test('pivot_multi_rhs')
  call print_array('B', packed_r, 2*n*nrhs)
  call print_int('info', info)
  call end_test()

end program
