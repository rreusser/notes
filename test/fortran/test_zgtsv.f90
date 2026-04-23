program test_zgtsv
  use test_utils
  implicit none
  complex*16 :: dl(10), d(10), du(10), b(10, 4)
  double precision :: dl_r(20), d_r(20), du_r(20)
  double precision :: b_pack(40)
  equivalence (dl, dl_r)
  equivalence (d, d_r)
  equivalence (du, du_r)
  integer :: info, i, j, n_val, nrhs_val

  ! Helper: pack b(1:n, 1:nrhs) into b_pack as interleaved re/im, column by column
  ! Each complex element b(i,j) → 2 doubles at b_pack((j-1)*2*n + (i-1)*2 + 1 : +2)
  ! But simpler: use equivalence on a temp copy

  ! Test 1: basic 5x5 tridiagonal, single RHS (real-valued for easy verification)
  n_val = 5
  nrhs_val = 1
  dl(1:4) = (/ (-1.0d0, 0.0d0), (-1.0d0, 0.0d0), (-1.0d0, 0.0d0), (-1.0d0, 0.0d0) /)
  d(1:5)  = (/ (2.0d0, 0.0d0), (2.0d0, 0.0d0), (2.0d0, 0.0d0), (2.0d0, 0.0d0), (2.0d0, 0.0d0) /)
  du(1:4) = (/ (-1.0d0, 0.0d0), (-1.0d0, 0.0d0), (-1.0d0, 0.0d0), (-1.0d0, 0.0d0) /)
  b(1:5, 1) = (/ (1.0d0, 0.0d0), (2.0d0, 0.0d0), (3.0d0, 0.0d0), (4.0d0, 0.0d0), (5.0d0, 0.0d0) /)
  call zgtsv(n_val, nrhs_val, dl, d, du, b, 10, info)
  call begin_test('basic_5x5_single_rhs')
  call print_int('N', n_val)
  call print_int('nrhs', nrhs_val)
  call print_array('d', d_r, 2*n_val)
  call print_array('dl', dl_r, 2*(n_val-1))
  call print_array('du', du_r, 2*(n_val-1))
  ! Pack b(1:5, 1) into b_pack
  call pack_b(b, 10, n_val, nrhs_val, b_pack)
  call print_array('b', b_pack, 2*n_val*nrhs_val)
  call print_int('info', info)
  call end_test()

  ! Test 2: 4x4 with 2 RHS, complex entries
  n_val = 4
  nrhs_val = 2
  dl(1:3) = (/ (1.0d0, 1.0d0), (1.0d0, -1.0d0), (2.0d0, 0.0d0) /)
  d(1:4)  = (/ (4.0d0, 0.0d0), (4.0d0, 1.0d0), (4.0d0, -1.0d0), (4.0d0, 0.0d0) /)
  du(1:3) = (/ (1.0d0, -1.0d0), (1.0d0, 1.0d0), (1.0d0, 0.0d0) /)
  b(1:4, 1) = (/ (6.0d0, -1.0d0), (10.0d0, 1.0d0), (10.0d0, -1.0d0), (7.0d0, 0.0d0) /)
  b(1:4, 2) = (/ (2.0d0, 0.0d0), (3.0d0, 1.0d0), (3.0d0, -1.0d0), (2.0d0, 0.0d0) /)
  call zgtsv(n_val, nrhs_val, dl, d, du, b, 10, info)
  call begin_test('multi_rhs_complex')
  call print_int('N', n_val)
  call print_int('nrhs', nrhs_val)
  call print_array('d', d_r, 2*n_val)
  call print_array('dl', dl_r, 2*(n_val-1))
  call print_array('du', du_r, 2*(n_val-1))
  call pack_b(b, 10, n_val, nrhs_val, b_pack)
  call print_array('b', b_pack, 2*n_val*nrhs_val)
  call print_int('info', info)
  call end_test()

  ! Test 3: n=1
  n_val = 1
  nrhs_val = 1
  d(1) = (5.0d0, 2.0d0)
  b(1, 1) = (10.0d0, 4.0d0)
  call zgtsv(n_val, nrhs_val, dl, d, du, b, 10, info)
  call begin_test('n_one')
  call print_int('N', n_val)
  call print_int('nrhs', nrhs_val)
  call print_array('d', d_r, 2)
  call pack_b(b, 10, n_val, nrhs_val, b_pack)
  call print_array('b', b_pack, 2)
  call print_int('info', info)
  call end_test()

  ! Test 4: n=0 (quick return)
  call zgtsv(0, 1, dl, d, du, b, 10, info)
  call begin_test('n_zero')
  call print_int('N', 0)
  call print_int('nrhs', 1)
  call print_int('info', info)
  call end_test()

  ! Test 5: singular matrix — D(1)=0 and DL(1)=0 → INFO=1
  dl(1:2) = (/ (0.0d0, 0.0d0), (0.0d0, 0.0d0) /)
  d(1:3)  = (/ (0.0d0, 0.0d0), (2.0d0, 0.0d0), (3.0d0, 0.0d0) /)
  du(1:2) = (/ (1.0d0, 0.0d0), (1.0d0, 0.0d0) /)
  b(1:3, 1) = (/ (1.0d0, 0.0d0), (2.0d0, 0.0d0), (3.0d0, 0.0d0) /)
  call zgtsv(3, 1, dl, d, du, b, 10, info)
  call begin_test('singular')
  call print_int('N', 3)
  call print_int('nrhs', 1)
  call print_int('info', info)
  call end_test()

  ! Test 6: pivoting case — subdiag larger than diagonal forces row swaps (real)
  n_val = 4
  nrhs_val = 1
  dl(1:3) = (/ (5.0d0, 0.0d0), (7.0d0, 0.0d0), (9.0d0, 0.0d0) /)
  d(1:4)  = (/ (1.0d0, 0.0d0), (3.0d0, 0.0d0), (2.0d0, 0.0d0), (1.0d0, 0.0d0) /)
  du(1:3) = (/ (2.0d0, 0.0d0), (4.0d0, 0.0d0), (6.0d0, 0.0d0) /)
  b(1:4, 1) = (/ (5.0d0, 0.0d0), (12.0d0, 0.0d0), (15.0d0, 0.0d0), (10.0d0, 0.0d0) /)
  call zgtsv(n_val, nrhs_val, dl, d, du, b, 10, info)
  call begin_test('pivoting')
  call print_int('N', n_val)
  call print_int('nrhs', nrhs_val)
  call print_array('d', d_r, 2*n_val)
  call print_array('dl', dl_r, 2*(n_val-1))
  call print_array('du', du_r, 2*(n_val-1))
  call pack_b(b, 10, n_val, nrhs_val, b_pack)
  call print_array('b', b_pack, 2*n_val*nrhs_val)
  call print_int('info', info)
  call end_test()

contains

  subroutine pack_b(b, ldb, n, nrhs, b_pack)
    integer, intent(in) :: ldb, n, nrhs
    complex*16, intent(in) :: b(ldb, *)
    double precision, intent(out) :: b_pack(*)
    double precision :: tmp(2)
    complex*16 :: ztmp
    equivalence (ztmp, tmp)
    integer :: i, j, k
    k = 1
    do j = 1, nrhs
      do i = 1, n
        ztmp = b(i, j)
        b_pack(k) = tmp(1)
        b_pack(k+1) = tmp(2)
        k = k + 2
      end do
    end do
  end subroutine

end program
