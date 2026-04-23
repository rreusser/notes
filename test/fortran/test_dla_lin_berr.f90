program test_dla_lin_berr
  use test_utils
  implicit none
  double precision, allocatable :: res(:,:), ayb(:,:), berr(:)
  integer :: n, nz, nrhs

  ! Test 1: nominal case, n=4, nrhs=2
  n = 4
  nz = 4
  nrhs = 2
  allocate(res(n, nrhs), ayb(n, nrhs), berr(nrhs))
  res(:, 1) = (/1.0d-10, 2.0d-10, 3.0d-10, 4.0d-10/)
  res(:, 2) = (/5.0d-10, 6.0d-10, 7.0d-10, 8.0d-10/)
  ayb(:, 1) = (/1.0d0, 2.0d0, 3.0d0, 4.0d0/)
  ayb(:, 2) = (/10.0d0, 20.0d0, 30.0d0, 40.0d0/)
  berr = -1.0d0
  call DLA_LIN_BERR(n, nz, nrhs, res, ayb, berr)
  call begin_test('basic')
  call print_array('berr', berr, nrhs)
  call end_test()
  deallocate(res, ayb, berr)

  ! Test 2: zero denominator (AYB column contains zeros -- skipped)
  n = 3
  nz = 3
  nrhs = 2
  allocate(res(n, nrhs), ayb(n, nrhs), berr(nrhs))
  res(:, 1) = (/1.0d-8, 2.0d-8, 3.0d-8/)
  res(:, 2) = (/4.0d-8, 5.0d-8, 6.0d-8/)
  ayb(:, 1) = (/0.0d0, 1.0d0, 0.0d0/)
  ayb(:, 2) = (/0.0d0, 0.0d0, 0.0d0/)
  berr = -1.0d0
  call DLA_LIN_BERR(n, nz, nrhs, res, ayb, berr)
  call begin_test('zero_denom')
  call print_array('berr', berr, nrhs)
  call end_test()
  deallocate(res, ayb, berr)

  ! Test 3: n=1, nrhs=1
  n = 1
  nz = 1
  nrhs = 1
  allocate(res(n, nrhs), ayb(n, nrhs), berr(nrhs))
  res(1, 1) = 1.0d-6
  ayb(1, 1) = 2.0d0
  berr = -1.0d0
  call DLA_LIN_BERR(n, nz, nrhs, res, ayb, berr)
  call begin_test('n_one')
  call print_array('berr', berr, nrhs)
  call end_test()
  deallocate(res, ayb, berr)

  ! Test 4: nrhs=3, varying scales
  n = 3
  nz = 3
  nrhs = 3
  allocate(res(n, nrhs), ayb(n, nrhs), berr(nrhs))
  res(:, 1) = (/1.0d0, 0.5d0, 0.25d0/)
  res(:, 2) = (/-1.0d-5, 2.0d-5, -3.0d-5/)
  res(:, 3) = (/1.0d0, 1.0d0, 1.0d0/)
  ayb(:, 1) = (/2.0d0, 4.0d0, 8.0d0/)
  ayb(:, 2) = (/1.0d0, 2.0d0, 3.0d0/)
  ayb(:, 3) = (/1.0d0, 0.5d0, 0.25d0/)
  berr = -1.0d0
  call DLA_LIN_BERR(n, nz, nrhs, res, ayb, berr)
  call begin_test('multi_rhs')
  call print_array('berr', berr, nrhs)
  call end_test()
  deallocate(res, ayb, berr)

  ! Test 5: nrhs=0 (quick return)
  n = 3
  nz = 3
  nrhs = 0
  allocate(res(max(n,1), max(nrhs,1)), ayb(max(n,1), max(nrhs,1)), berr(1))
  berr(1) = 99.0d0
  call DLA_LIN_BERR(n, nz, nrhs, res, ayb, berr)
  call begin_test('nrhs_zero')
  call print_array('berr', berr, 1)
  call end_test()
  deallocate(res, ayb, berr)

end program
