program test_zlaqr0
  use test_utils
  implicit none

  integer, parameter :: NMAX = 16
  complex*16 :: H(NMAX, NMAX), Z(NMAX, NMAX), W(NMAX)
  complex*16 :: WORK(NMAX*NMAX)
  double precision :: W_r(2*NMAX)
  equivalence (W, W_r)
  complex*16 :: Hpk(NMAX*NMAX), Zpk(NMAX*NMAX)
  double precision :: Hpk_r(2*NMAX*NMAX), Zpk_r(2*NMAX*NMAX)
  equivalence (Hpk, Hpk_r)
  equivalence (Zpk, Zpk_r)
  integer :: info, n, ilo, ihi, ldh, ldz, iloz, ihiz, lwork
  integer :: i, j

  ldh = NMAX
  ldz = NMAX

  ! ==================================================================
  ! Test 1: N=0 quick return
  ! ==================================================================
  n = 0
  ilo = 1
  ihi = 0
  iloz = 1
  ihiz = 0
  lwork = 1

  call ZLAQR0(.TRUE., .FALSE., n, ilo, ihi, H, ldh, W, iloz, ihiz, &
               Z, ldz, WORK, lwork, info)

  call begin_test('n_eq_0')
  call print_int('info', info)
  call end_test()

  ! ==================================================================
  ! Test 2: N=1
  ! ==================================================================
  n = 1
  ilo = 1
  ihi = 1
  iloz = 1
  ihiz = 1
  lwork = n * n

  H = (0.0d0, 0.0d0)
  Z = (0.0d0, 0.0d0)
  W = (0.0d0, 0.0d0)

  H(1,1) = (5.0d0, -3.0d0)
  Z(1,1) = (1.0d0, 0.0d0)

  call ZLAQR0(.TRUE., .TRUE., n, ilo, ihi, H, ldh, W, iloz, ihiz, &
               Z, ldz, WORK, lwork, info)

  call begin_test('n_eq_1')
  call print_int('info', info)
  call print_array('w', W_r, 2*n)
  call end_test()

  ! ==================================================================
  ! Test 3: 6x6 (small, uses zlahqr path), Schur + Z
  ! ==================================================================
  n = 6
  ilo = 1
  ihi = 6
  iloz = 1
  ihiz = 6
  lwork = n * n

  H = (0.0d0, 0.0d0)
  Z = (0.0d0, 0.0d0)
  W = (0.0d0, 0.0d0)
  WORK = (0.0d0, 0.0d0)

  H(1,1) = (6.0d0, 1.0d0)
  H(1,2) = (1.0d0, -0.5d0)
  H(1,3) = (0.5d0, 0.0d0)
  H(1,4) = (0.25d0, 0.1d0)
  H(1,5) = (0.1d0, 0.0d0)
  H(1,6) = (0.05d0, -0.05d0)
  H(2,1) = (1.0d0, 0.0d0)
  H(2,2) = (5.0d0, -1.0d0)
  H(2,3) = (1.0d0, 0.5d0)
  H(2,4) = (0.5d0, 0.0d0)
  H(2,5) = (0.25d0, -0.1d0)
  H(2,6) = (0.1d0, 0.0d0)
  H(3,2) = (0.8d0, 0.2d0)
  H(3,3) = (4.0d0, 0.5d0)
  H(3,4) = (1.0d0, -0.5d0)
  H(3,5) = (0.5d0, 0.0d0)
  H(3,6) = (0.25d0, 0.1d0)
  H(4,3) = (0.6d0, -0.1d0)
  H(4,4) = (3.0d0, -0.5d0)
  H(4,5) = (1.0d0, 0.5d0)
  H(4,6) = (0.5d0, 0.0d0)
  H(5,4) = (0.4d0, 0.15d0)
  H(5,5) = (2.0d0, 0.0d0)
  H(5,6) = (1.0d0, -0.5d0)
  H(6,5) = (0.2d0, -0.1d0)
  H(6,6) = (1.0d0, 1.0d0)

  do i = 1, n
    Z(i,i) = (1.0d0, 0.0d0)
  end do

  call ZLAQR0(.TRUE., .TRUE., n, ilo, ihi, H, ldh, W, iloz, ihiz, &
               Z, ldz, WORK, lwork, info)

  call begin_test('6x6_schur_with_Z')
  call print_int('info', info)
  call print_int('n', n)
  call print_array('w', W_r, 2*n)
  do j = 1, n
    do i = 1, n
      Hpk((j-1)*n + i) = H(i,j)
      Zpk((j-1)*n + i) = Z(i,j)
    end do
  end do
  call print_array('H', Hpk_r, 2*n*n)
  call print_array('Z', Zpk_r, 2*n*n)
  call end_test()

  ! ==================================================================
  ! Test 4: 6x6 eigenvalues only
  ! ==================================================================
  n = 6
  ilo = 1
  ihi = 6
  iloz = 1
  ihiz = 6
  lwork = n * n

  H = (0.0d0, 0.0d0)
  Z = (0.0d0, 0.0d0)
  W = (0.0d0, 0.0d0)
  WORK = (0.0d0, 0.0d0)

  H(1,1) = (6.0d0, 1.0d0)
  H(1,2) = (1.0d0, -0.5d0)
  H(1,3) = (0.5d0, 0.0d0)
  H(1,4) = (0.25d0, 0.1d0)
  H(1,5) = (0.1d0, 0.0d0)
  H(1,6) = (0.05d0, -0.05d0)
  H(2,1) = (1.0d0, 0.0d0)
  H(2,2) = (5.0d0, -1.0d0)
  H(2,3) = (1.0d0, 0.5d0)
  H(2,4) = (0.5d0, 0.0d0)
  H(2,5) = (0.25d0, -0.1d0)
  H(2,6) = (0.1d0, 0.0d0)
  H(3,2) = (0.8d0, 0.2d0)
  H(3,3) = (4.0d0, 0.5d0)
  H(3,4) = (1.0d0, -0.5d0)
  H(3,5) = (0.5d0, 0.0d0)
  H(3,6) = (0.25d0, 0.1d0)
  H(4,3) = (0.6d0, -0.1d0)
  H(4,4) = (3.0d0, -0.5d0)
  H(4,5) = (1.0d0, 0.5d0)
  H(4,6) = (0.5d0, 0.0d0)
  H(5,4) = (0.4d0, 0.15d0)
  H(5,5) = (2.0d0, 0.0d0)
  H(5,6) = (1.0d0, -0.5d0)
  H(6,5) = (0.2d0, -0.1d0)
  H(6,6) = (1.0d0, 1.0d0)

  call ZLAQR0(.FALSE., .FALSE., n, ilo, ihi, H, ldh, W, iloz, ihiz, &
               Z, ldz, WORK, lwork, info)

  call begin_test('6x6_eig_only')
  call print_int('info', info)
  call print_array('w', W_r, 2*n)
  call end_test()

  ! ==================================================================
  ! Test 5: 15x15 (multishift path with recursive deflation)
  ! ==================================================================
  n = 15
  ilo = 1
  ihi = 15
  iloz = 1
  ihiz = 15
  lwork = n * n

  H = (0.0d0, 0.0d0)
  Z = (0.0d0, 0.0d0)
  W = (0.0d0, 0.0d0)
  WORK = (0.0d0, 0.0d0)

  ! Build a diagonally dominant upper Hessenberg 15x15
  do i = 1, n
    H(i,i) = dcmplx(dble(n - i + 1) * 1.0d0, dble(i) * 0.1d0)
    do j = i+1, min(i+3, n)
      H(i,j) = dcmplx(0.5d0 / dble(j-i), 0.1d0 * (-1)**j)
    end do
    if (i < n) then
      H(i+1,i) = dcmplx(0.3d0 + 0.1d0 * dble(i), 0.05d0 * (-1)**i)
    end if
  end do

  do i = 1, n
    Z(i,i) = (1.0d0, 0.0d0)
  end do

  call ZLAQR0(.TRUE., .TRUE., n, ilo, ihi, H, ldh, W, iloz, ihiz, &
               Z, ldz, WORK, lwork, info)

  call begin_test('15x15_multishift')
  call print_int('info', info)
  call print_int('n', n)
  call print_array('w', W_r, 2*n)
  do j = 1, n
    do i = 1, n
      Hpk((j-1)*n + i) = H(i,j)
    end do
  end do
  call print_array('H', Hpk_r, 2*n*n)
  call end_test()

end program
