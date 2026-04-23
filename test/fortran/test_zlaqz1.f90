program test_zlaqz1
  use test_utils
  implicit none

  integer, parameter :: NMAX = 5
  complex*16 :: A(NMAX, NMAX), B(NMAX, NMAX)
  complex*16 :: Q(NMAX, NMAX), Z(NMAX, NMAX)
  double precision :: A_r(2*NMAX*NMAX), B_r(2*NMAX*NMAX)
  double precision :: Q_r(2*NMAX*NMAX), Z_r(2*NMAX*NMAX)
  equivalence (A, A_r)
  equivalence (B, B_r)
  equivalence (Q, Q_r)
  equivalence (Z, Z_r)
  integer :: lda, ldb, ldq, ldz
  integer :: i, j, k, istartm, istopm, ihi, nq, qstart, nz, zstart

  lda = NMAX
  ldb = NMAX
  ldq = NMAX
  ldz = NMAX

  ! ==================================================================
  ! Test 1: Normal operation, move bulge down (K+1 < IHI).
  ! N=5 pencil; bulge at (k+1,k) = (3,2). istartm=1, istopm=5.
  ! ==================================================================
  do j = 1, NMAX
    do i = 1, NMAX
      A(i,j) = dcmplx( dble(i + 2*j), dble(i - j) * 0.3d0 )
      B(i,j) = dcmplx( 0.0d0, 0.0d0 )
      Q(i,j) = dcmplx( 0.0d0, 0.0d0 )
      Z(i,j) = dcmplx( 0.0d0, 0.0d0 )
    end do
    Q(j,j) = dcmplx( 1.0d0, 0.0d0 )
    Z(j,j) = dcmplx( 1.0d0, 0.0d0 )
  end do
  ! B upper triangular (with bulge at (3,2))
  do j = 1, NMAX
    do i = 1, j
      B(i,j) = dcmplx( dble(i + j) * 0.5d0, dble(j - i) * 0.2d0 )
    end do
  end do
  ! Inject bulge at B(3,2) and A(3,2):
  B(3,2) = dcmplx( 0.7d0, -0.4d0 )
  A(3,2) = dcmplx( 1.5d0, 0.6d0 )
  ! Bulge at (k+1,k) means k=2 in 1-based Fortran (k=1 in 0-based)
  k = 2
  istartm = 1
  istopm = NMAX
  ihi = NMAX
  nq = NMAX
  qstart = 1
  nz = NMAX
  zstart = 1

  call ZLAQZ1(.true., .true., k, istartm, istopm, ihi, A, lda, B, ldb, &
              nq, qstart, Q, ldq, nz, zstart, Z, ldz)

  call begin_test('move_bulge_down')
  call print_array('A', A_r, 2*NMAX*NMAX)
  call print_array('B', B_r, 2*NMAX*NMAX)
  call print_array('Q', Q_r, 2*NMAX*NMAX)
  call print_array('Z', Z_r, 2*NMAX*NMAX)
  call end_test()

  ! ==================================================================
  ! Test 2: Edge case, K+1 == IHI.
  ! N=5; k=4 (Fortran 1-based), ihi=5. Trigger ZLARTG on B(IHI,IHI), B(IHI,IHI-1).
  ! ==================================================================
  do j = 1, NMAX
    do i = 1, NMAX
      A(i,j) = dcmplx( dble(2*i + j), dble(j - i) * 0.2d0 )
      B(i,j) = dcmplx( 0.0d0, 0.0d0 )
      Q(i,j) = dcmplx( 0.0d0, 0.0d0 )
      Z(i,j) = dcmplx( 0.0d0, 0.0d0 )
    end do
    Q(j,j) = dcmplx( 1.0d0, 0.0d0 )
    Z(j,j) = dcmplx( 1.0d0, 0.0d0 )
  end do
  do j = 1, NMAX
    do i = 1, j
      B(i,j) = dcmplx( dble(i*j) * 0.4d0 + 1.0d0, dble(j - i) * 0.1d0 )
    end do
  end do
  ! Bulge at B(IHI, IHI-1) = B(5,4)
  B(5,4) = dcmplx( 0.5d0, 0.3d0 )
  k = 4
  istartm = 1
  istopm = NMAX
  ihi = 5
  nq = NMAX
  qstart = 1
  nz = NMAX
  zstart = 1

  call ZLAQZ1(.true., .true., k, istartm, istopm, ihi, A, lda, B, ldb, &
              nq, qstart, Q, ldq, nz, zstart, Z, ldz)

  call begin_test('edge_remove_shift')
  call print_array('A', A_r, 2*NMAX*NMAX)
  call print_array('B', B_r, 2*NMAX*NMAX)
  call print_array('Q', Q_r, 2*NMAX*NMAX)
  call print_array('Z', Z_r, 2*NMAX*NMAX)
  call end_test()

  ! ==================================================================
  ! Test 3: Normal operation with ILQ=.false., ILZ=.false. (no Q/Z update)
  ! ==================================================================
  do j = 1, NMAX
    do i = 1, NMAX
      A(i,j) = dcmplx( dble(i + 3*j), dble(i*j) * 0.1d0 )
      B(i,j) = dcmplx( 0.0d0, 0.0d0 )
      Q(i,j) = dcmplx( 0.0d0, 0.0d0 )
      Z(i,j) = dcmplx( 0.0d0, 0.0d0 )
    end do
    Q(j,j) = dcmplx( 1.0d0, 0.0d0 )
    Z(j,j) = dcmplx( 1.0d0, 0.0d0 )
  end do
  do j = 1, NMAX
    do i = 1, j
      B(i,j) = dcmplx( dble(i+j) * 0.3d0 + 0.5d0, dble(j*i) * 0.05d0 )
    end do
  end do
  B(2,1) = dcmplx( 0.6d0, -0.3d0 )
  A(2,1) = dcmplx( 1.2d0, 0.4d0 )
  k = 1
  istartm = 1
  istopm = NMAX
  ihi = NMAX
  nq = NMAX
  qstart = 1
  nz = NMAX
  zstart = 1

  call ZLAQZ1(.false., .false., k, istartm, istopm, ihi, A, lda, B, ldb, &
              nq, qstart, Q, ldq, nz, zstart, Z, ldz)

  call begin_test('move_bulge_no_qz')
  call print_array('A', A_r, 2*NMAX*NMAX)
  call print_array('B', B_r, 2*NMAX*NMAX)
  call print_array('Q', Q_r, 2*NMAX*NMAX)
  call print_array('Z', Z_r, 2*NMAX*NMAX)
  call end_test()

end program
