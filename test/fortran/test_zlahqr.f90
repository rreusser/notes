program test_zlahqr
  use test_utils
  implicit none

  integer, parameter :: NMAX = 6
  complex*16 :: H(NMAX, NMAX), Z(NMAX, NMAX), W(NMAX)
  double precision :: W_r(2*NMAX)
  equivalence (W, W_r)
  ! Temporary contiguous arrays for printing n x n submatrix
  double precision :: Htmp(2*NMAX*NMAX), Ztmp(2*NMAX*NMAX)
  integer :: info, n, ilo, ihi, ldh, ldz, iloz, ihiz
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

  call ZLAHQR(.TRUE., .FALSE., n, ilo, ihi, H, ldh, W, iloz, ihiz, &
               Z, ldz, info)

  call begin_test('n_eq_0')
  call print_int('info', info)
  call end_test()

  ! ==================================================================
  ! Test 2: N=1, trivial eigenvalue
  ! ==================================================================
  n = 1
  ilo = 1
  ihi = 1
  iloz = 1
  ihiz = 1

  H = (0.0d0, 0.0d0)
  Z = (0.0d0, 0.0d0)
  W = (0.0d0, 0.0d0)

  H(1,1) = (3.0d0, 2.0d0)
  Z(1,1) = (1.0d0, 0.0d0)

  call ZLAHQR(.TRUE., .TRUE., n, ilo, ihi, H, ldh, W, iloz, ihiz, &
               Z, ldz, info)

  call begin_test('n_eq_1')
  call print_int('info', info)
  call print_array('w', W_r, 2*n)
  call copy_submatrix(H, NMAX, n, Htmp)
  call print_array('H', Htmp, 2*n*n)
  call end_test()

  ! ==================================================================
  ! Test 3: 4x4 Hessenberg, WANTT=true, WANTZ=true
  ! Well-conditioned upper Hessenberg matrix
  ! ==================================================================
  n = 4
  ilo = 1
  ihi = 4
  iloz = 1
  ihiz = 4

  H = (0.0d0, 0.0d0)
  Z = (0.0d0, 0.0d0)
  W = (0.0d0, 0.0d0)

  ! Upper Hessenberg: H(i,j) = 0 for i > j+1
  H(1,1) = (4.0d0, 1.0d0)
  H(1,2) = (2.0d0, -1.0d0)
  H(1,3) = (1.0d0, 0.5d0)
  H(1,4) = (0.5d0, 0.0d0)
  H(2,1) = (1.0d0, 0.0d0)
  H(2,2) = (3.0d0, -0.5d0)
  H(2,3) = (1.5d0, 1.0d0)
  H(2,4) = (1.0d0, -0.5d0)
  H(3,2) = (0.5d0, 0.25d0)
  H(3,3) = (2.0d0, 0.0d0)
  H(3,4) = (2.0d0, 1.0d0)
  H(4,3) = (0.25d0, -0.1d0)
  H(4,4) = (1.0d0, 0.5d0)

  ! Initialize Z to identity
  do i = 1, n
    Z(i,i) = (1.0d0, 0.0d0)
  end do

  call ZLAHQR(.TRUE., .TRUE., n, ilo, ihi, H, ldh, W, iloz, ihiz, &
               Z, ldz, info)

  call begin_test('4x4_schur_with_Z')
  call print_int('info', info)
  call print_int('n', n)
  call print_array('w', W_r, 2*n)
  call copy_submatrix(H, NMAX, n, Htmp)
  call print_array('H', Htmp, 2*n*n)
  call copy_submatrix(Z, NMAX, n, Ztmp)
  call print_array('Z', Ztmp, 2*n*n)
  call end_test()

  ! ==================================================================
  ! Test 4: 4x4 Hessenberg, WANTT=true, WANTZ=false (eigenvalues + Schur form, no Z)
  ! ==================================================================
  n = 4
  ilo = 1
  ihi = 4
  iloz = 1
  ihiz = 4

  H = (0.0d0, 0.0d0)
  Z = (0.0d0, 0.0d0)
  W = (0.0d0, 0.0d0)

  H(1,1) = (4.0d0, 1.0d0)
  H(1,2) = (2.0d0, -1.0d0)
  H(1,3) = (1.0d0, 0.5d0)
  H(1,4) = (0.5d0, 0.0d0)
  H(2,1) = (1.0d0, 0.0d0)
  H(2,2) = (3.0d0, -0.5d0)
  H(2,3) = (1.5d0, 1.0d0)
  H(2,4) = (1.0d0, -0.5d0)
  H(3,2) = (0.5d0, 0.25d0)
  H(3,3) = (2.0d0, 0.0d0)
  H(3,4) = (2.0d0, 1.0d0)
  H(4,3) = (0.25d0, -0.1d0)
  H(4,4) = (1.0d0, 0.5d0)

  call ZLAHQR(.TRUE., .FALSE., n, ilo, ihi, H, ldh, W, iloz, ihiz, &
               Z, ldz, info)

  call begin_test('4x4_schur_no_Z')
  call print_int('info', info)
  call print_array('w', W_r, 2*n)
  call copy_submatrix(H, NMAX, n, Htmp)
  call print_array('H', Htmp, 2*n*n)
  call end_test()

  ! ==================================================================
  ! Test 5: 4x4, eigenvalues only (WANTT=false, WANTZ=false)
  ! ==================================================================
  n = 4
  ilo = 1
  ihi = 4
  iloz = 1
  ihiz = 4

  H = (0.0d0, 0.0d0)
  Z = (0.0d0, 0.0d0)
  W = (0.0d0, 0.0d0)

  H(1,1) = (4.0d0, 1.0d0)
  H(1,2) = (2.0d0, -1.0d0)
  H(1,3) = (1.0d0, 0.5d0)
  H(1,4) = (0.5d0, 0.0d0)
  H(2,1) = (1.0d0, 0.0d0)
  H(2,2) = (3.0d0, -0.5d0)
  H(2,3) = (1.5d0, 1.0d0)
  H(2,4) = (1.0d0, -0.5d0)
  H(3,2) = (0.5d0, 0.25d0)
  H(3,3) = (2.0d0, 0.0d0)
  H(3,4) = (2.0d0, 1.0d0)
  H(4,3) = (0.25d0, -0.1d0)
  H(4,4) = (1.0d0, 0.5d0)

  call ZLAHQR(.FALSE., .FALSE., n, ilo, ihi, H, ldh, W, iloz, ihiz, &
               Z, ldz, info)

  call begin_test('4x4_eig_only')
  call print_int('info', info)
  call print_array('w', W_r, 2*n)
  call end_test()

  ! ==================================================================
  ! Test 6: Already upper triangular (should return immediately)
  ! ==================================================================
  n = 3
  ilo = 1
  ihi = 3
  iloz = 1
  ihiz = 3

  H = (0.0d0, 0.0d0)
  Z = (0.0d0, 0.0d0)
  W = (0.0d0, 0.0d0)

  ! Upper triangular (all subdiag zero) => eigenvalues are diagonal entries
  H(1,1) = (5.0d0, 1.0d0)
  H(1,2) = (2.0d0, 0.0d0)
  H(1,3) = (1.0d0, -1.0d0)
  H(2,2) = (3.0d0, -2.0d0)
  H(2,3) = (0.5d0, 0.5d0)
  H(3,3) = (1.0d0, 4.0d0)

  do i = 1, n
    Z(i,i) = (1.0d0, 0.0d0)
  end do

  call ZLAHQR(.TRUE., .TRUE., n, ilo, ihi, H, ldh, W, iloz, ihiz, &
               Z, ldz, info)

  call begin_test('already_triangular')
  call print_int('info', info)
  call print_array('w', W_r, 2*n)
  call copy_submatrix(H, NMAX, n, Htmp)
  call print_array('H', Htmp, 2*n*n)
  call end_test()

  ! ==================================================================
  ! Test 7: Partial active block (ilo=2, ihi=4 on a 4x4)
  ! ==================================================================
  n = 4
  ilo = 2
  ihi = 4
  iloz = 1
  ihiz = 4

  H = (0.0d0, 0.0d0)
  Z = (0.0d0, 0.0d0)
  W = (0.0d0, 0.0d0)

  ! Element (1,1) is already converged
  H(1,1) = (10.0d0, 0.0d0)
  H(1,2) = (1.0d0, 0.5d0)
  H(1,3) = (0.5d0, -0.5d0)
  H(1,4) = (0.25d0, 0.0d0)
  ! Active block starts at (2,2)
  H(2,2) = (4.0d0, 1.0d0)
  H(2,3) = (2.0d0, -1.0d0)
  H(2,4) = (1.0d0, 0.5d0)
  H(3,2) = (1.0d0, 0.0d0)
  H(3,3) = (3.0d0, -0.5d0)
  H(3,4) = (1.5d0, 1.0d0)
  H(4,3) = (0.5d0, 0.25d0)
  H(4,4) = (2.0d0, 0.0d0)

  do i = 1, n
    Z(i,i) = (1.0d0, 0.0d0)
  end do

  call ZLAHQR(.TRUE., .TRUE., n, ilo, ihi, H, ldh, W, iloz, ihiz, &
               Z, ldz, info)

  call begin_test('partial_active_block')
  call print_int('info', info)
  call print_int('n', n)
  call print_array('w', W_r, 2*n)
  call copy_submatrix(H, NMAX, n, Htmp)
  call print_array('H', Htmp, 2*n*n)
  call copy_submatrix(Z, NMAX, n, Ztmp)
  call print_array('Z', Ztmp, 2*n*n)
  call end_test()

  ! ==================================================================
  ! Test 8: 6x6 Hessenberg with complex entries
  ! ==================================================================
  n = 6
  ilo = 1
  ihi = 6
  iloz = 1
  ihiz = 6

  H = (0.0d0, 0.0d0)
  Z = (0.0d0, 0.0d0)
  W = (0.0d0, 0.0d0)

  ! Diagonally dominant upper Hessenberg
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

  call ZLAHQR(.TRUE., .TRUE., n, ilo, ihi, H, ldh, W, iloz, ihiz, &
               Z, ldz, info)

  call begin_test('6x6_full')
  call print_int('info', info)
  call print_int('n', n)
  call print_array('w', W_r, 2*n)
  call copy_submatrix(H, NMAX, n, Htmp)
  call print_array('H', Htmp, 2*n*n)
  call copy_submatrix(Z, NMAX, n, Ztmp)
  call print_array('Z', Ztmp, 2*n*n)
  call end_test()

contains

  subroutine copy_submatrix(A, lda, n, Aflat)
    ! Copy the n x n leading submatrix of A (complex, with leading dim lda)
    ! into a contiguous double precision array Aflat (interleaved re/im)
    implicit none
    integer, intent(in) :: lda, n
    complex*16, intent(in) :: A(lda, *)
    double precision, intent(out) :: Aflat(*)
    integer :: ii, jj, k
    k = 1
    do jj = 1, n
      do ii = 1, n
        Aflat(k)   = dble(A(ii, jj))
        Aflat(k+1) = dimag(A(ii, jj))
        k = k + 2
      end do
    end do
  end subroutine

end program
