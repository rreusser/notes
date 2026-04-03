program test_zhpcon
  use test_utils
  implicit none

  integer, parameter :: NMAX = 6
  integer, parameter :: APMAX = NMAX*(NMAX+1)/2
  complex*16 :: AP(APMAX), WORK(2*NMAX)
  double precision :: AP_r(2*APMAX), WORK_r(2*2*NMAX)
  equivalence (AP, AP_r)
  equivalence (WORK, WORK_r)
  integer :: IPIV(NMAX), INFO, n, nn
  double precision :: ANORM, RCOND

  ! Test 1: Upper, 3x3, well-conditioned Hermitian
  ! Hermitian: diagonal is real, A(i,j) = conjg(A(j,i))
  n = 3
  nn = n*(n+1)/2
  AP = (0.0d0, 0.0d0)
  ! Upper packed: (1,1), (1,2), (2,2), (1,3), (2,3), (3,3)
  AP(1) = (4.0d0, 0.0d0)    ! A(1,1) real
  AP(2) = (1.0d0, 2.0d0)    ! A(1,2)
  AP(3) = (5.0d0, 0.0d0)    ! A(2,2) real
  AP(4) = (2.0d0, -1.0d0)   ! A(1,3)
  AP(5) = (3.0d0, 1.0d0)    ! A(2,3)
  AP(6) = (6.0d0, 0.0d0)    ! A(3,3) real
  call packed_herm_1norm('U', n, AP, ANORM)
  call ZHPTRF('U', n, AP, IPIV, INFO)
  call begin_test('upper_well_cond')
  call print_scalar('anorm', ANORM)
  call print_int('info_trf', INFO)
  call print_array('AP_factored', AP_r, 2*nn)
  call print_int_array('ipiv', IPIV, n)
  call ZHPCON('U', n, AP, IPIV, ANORM, RCOND, WORK, INFO)
  call print_scalar('rcond', RCOND)
  call print_int('info', INFO)
  call end_test()

  ! Test 2: Lower, 3x3, well-conditioned Hermitian
  n = 3
  nn = n*(n+1)/2
  AP = (0.0d0, 0.0d0)
  ! Lower packed: (1,1), (2,1), (3,1), (2,2), (3,2), (3,3)
  AP(1) = (4.0d0, 0.0d0)    ! A(1,1) real
  AP(2) = (1.0d0, -2.0d0)   ! A(2,1) = conjg(A(1,2))
  AP(3) = (2.0d0, 1.0d0)    ! A(3,1) = conjg(A(1,3))
  AP(4) = (5.0d0, 0.0d0)    ! A(2,2) real
  AP(5) = (3.0d0, -1.0d0)   ! A(3,2) = conjg(A(2,3))
  AP(6) = (6.0d0, 0.0d0)    ! A(3,3) real
  call packed_herm_1norm('L', n, AP, ANORM)
  call ZHPTRF('L', n, AP, IPIV, INFO)
  call begin_test('lower_well_cond')
  call print_scalar('anorm', ANORM)
  call print_int('info_trf', INFO)
  call print_array('AP_factored', AP_r, 2*nn)
  call print_int_array('ipiv', IPIV, n)
  call ZHPCON('L', n, AP, IPIV, ANORM, RCOND, WORK, INFO)
  call print_scalar('rcond', RCOND)
  call print_int('info', INFO)
  call end_test()

  ! Test 3: N=0
  call ZHPCON('U', 0, AP, IPIV, 0.0d0, RCOND, WORK, INFO)
  call begin_test('n_zero')
  call print_scalar('rcond', RCOND)
  call print_int('info', INFO)
  call end_test()

  ! Test 4: N=1 (upper)
  n = 1
  nn = 1
  AP(1) = (5.0d0, 0.0d0)  ! Hermitian diagonal must be real
  ANORM = ABS(AP(1))
  call ZHPTRF('U', n, AP, IPIV, INFO)
  call begin_test('n_one_upper')
  call print_scalar('anorm', ANORM)
  call print_array('AP_factored', AP_r, 2*nn)
  call print_int_array('ipiv', IPIV, n)
  call ZHPCON('U', n, AP, IPIV, ANORM, RCOND, WORK, INFO)
  call print_scalar('rcond', RCOND)
  call print_int('info', INFO)
  call end_test()

  ! Test 5: N=1 (lower)
  n = 1
  nn = 1
  AP(1) = (3.0d0, 0.0d0)  ! Hermitian diagonal must be real
  ANORM = ABS(AP(1))
  call ZHPTRF('L', n, AP, IPIV, INFO)
  call begin_test('n_one_lower')
  call print_scalar('anorm', ANORM)
  call print_array('AP_factored', AP_r, 2*nn)
  call print_int_array('ipiv', IPIV, n)
  call ZHPCON('L', n, AP, IPIV, ANORM, RCOND, WORK, INFO)
  call print_scalar('rcond', RCOND)
  call print_int('info', INFO)
  call end_test()

  ! Test 6: 3x3 identity (upper, rcond=1)
  n = 3
  nn = n*(n+1)/2
  AP = (0.0d0, 0.0d0)
  AP(1) = (1.0d0, 0.0d0)
  AP(3) = (1.0d0, 0.0d0)
  AP(6) = (1.0d0, 0.0d0)
  ANORM = 1.0d0
  call ZHPTRF('U', n, AP, IPIV, INFO)
  call begin_test('identity_upper')
  call print_scalar('anorm', ANORM)
  call print_int('info_trf', INFO)
  call print_array('AP_factored', AP_r, 2*nn)
  call print_int_array('ipiv', IPIV, n)
  call ZHPCON('U', n, AP, IPIV, ANORM, RCOND, WORK, INFO)
  call print_scalar('rcond', RCOND)
  call print_int('info', INFO)
  call end_test()

  ! Test 7: 3x3 identity (lower, rcond=1)
  n = 3
  nn = n*(n+1)/2
  AP = (0.0d0, 0.0d0)
  AP(1) = (1.0d0, 0.0d0)
  AP(4) = (1.0d0, 0.0d0)
  AP(6) = (1.0d0, 0.0d0)
  ANORM = 1.0d0
  call ZHPTRF('L', n, AP, IPIV, INFO)
  call begin_test('identity_lower')
  call print_scalar('anorm', ANORM)
  call print_int('info_trf', INFO)
  call print_array('AP_factored', AP_r, 2*nn)
  call print_int_array('ipiv', IPIV, n)
  call ZHPCON('L', n, AP, IPIV, ANORM, RCOND, WORK, INFO)
  call print_scalar('rcond', RCOND)
  call print_int('info', INFO)
  call end_test()

  ! Test 8: Ill-conditioned diagonal (upper)
  ! A = diag(1, 1, 1e-15)
  n = 3
  nn = n*(n+1)/2
  AP = (0.0d0, 0.0d0)
  AP(1) = (1.0d0, 0.0d0)
  AP(3) = (1.0d0, 0.0d0)
  AP(6) = (1.0d-15, 0.0d0)
  ANORM = 1.0d0
  call ZHPTRF('U', n, AP, IPIV, INFO)
  call begin_test('ill_cond_upper')
  call print_scalar('anorm', ANORM)
  call print_int('info_trf', INFO)
  call print_array('AP_factored', AP_r, 2*nn)
  call print_int_array('ipiv', IPIV, n)
  call ZHPCON('U', n, AP, IPIV, ANORM, RCOND, WORK, INFO)
  call print_scalar('rcond', RCOND)
  call print_int('info', INFO)
  call end_test()

  ! Test 9: Singular 3x3 Hermitian (upper) - rank deficient
  ! Hermitian rank-1: v*v^H where v = [1, 1, 1]^T
  n = 3
  nn = n*(n+1)/2
  AP = (0.0d0, 0.0d0)
  AP(1) = (1.0d0, 0.0d0)   ! A(1,1)
  AP(2) = (1.0d0, 0.0d0)   ! A(1,2)
  AP(3) = (1.0d0, 0.0d0)   ! A(2,2)
  AP(4) = (1.0d0, 0.0d0)   ! A(1,3)
  AP(5) = (1.0d0, 0.0d0)   ! A(2,3)
  AP(6) = (1.0d0, 0.0d0)   ! A(3,3)
  call packed_herm_1norm('U', n, AP, ANORM)
  call ZHPTRF('U', n, AP, IPIV, INFO)
  call begin_test('singular_upper')
  call print_scalar('anorm', ANORM)
  call print_int('info_trf', INFO)
  call print_array('AP_factored', AP_r, 2*nn)
  call print_int_array('ipiv', IPIV, n)
  call ZHPCON('U', n, AP, IPIV, ANORM, RCOND, WORK, INFO)
  call print_scalar('rcond', RCOND)
  call print_int('info', INFO)
  call end_test()

  ! Test 10: 4x4 well-conditioned Hermitian (upper)
  n = 4
  nn = n*(n+1)/2
  AP = (0.0d0, 0.0d0)
  ! Upper packed: (1,1),(1,2),(2,2),(1,3),(2,3),(3,3),(1,4),(2,4),(3,4),(4,4)
  AP(1)  = (10.0d0, 0.0d0)    ! A(1,1) real
  AP(2)  = (1.0d0, 1.0d0)     ! A(1,2)
  AP(3)  = (8.0d0, 0.0d0)     ! A(2,2) real
  AP(4)  = (0.0d0, 0.0d0)     ! A(1,3)
  AP(5)  = (1.0d0, 0.0d0)     ! A(2,3)
  AP(6)  = (6.0d0, 0.0d0)     ! A(3,3) real
  AP(7)  = (0.0d0, 0.0d0)     ! A(1,4)
  AP(8)  = (0.0d0, 0.0d0)     ! A(2,4)
  AP(9)  = (1.0d0, -1.0d0)    ! A(3,4)
  AP(10) = (5.0d0, 0.0d0)     ! A(4,4) real
  call packed_herm_1norm('U', n, AP, ANORM)
  call ZHPTRF('U', n, AP, IPIV, INFO)
  call begin_test('4x4_upper')
  call print_scalar('anorm', ANORM)
  call print_int('info_trf', INFO)
  call print_array('AP_factored', AP_r, 2*nn)
  call print_int_array('ipiv', IPIV, n)
  call ZHPCON('U', n, AP, IPIV, ANORM, RCOND, WORK, INFO)
  call print_scalar('rcond', RCOND)
  call print_int('info', INFO)
  call end_test()

  ! Test 11: 4x4 well-conditioned Hermitian (lower) - same matrix
  n = 4
  nn = n*(n+1)/2
  AP = (0.0d0, 0.0d0)
  ! Lower packed: (1,1),(2,1),(3,1),(4,1),(2,2),(3,2),(4,2),(3,3),(4,3),(4,4)
  AP(1)  = (10.0d0, 0.0d0)    ! A(1,1) real
  AP(2)  = (1.0d0, -1.0d0)    ! A(2,1) = conjg(A(1,2))
  AP(3)  = (0.0d0, 0.0d0)     ! A(3,1)
  AP(4)  = (0.0d0, 0.0d0)     ! A(4,1)
  AP(5)  = (8.0d0, 0.0d0)     ! A(2,2) real
  AP(6)  = (1.0d0, 0.0d0)     ! A(3,2)
  AP(7)  = (0.0d0, 0.0d0)     ! A(4,2)
  AP(8)  = (6.0d0, 0.0d0)     ! A(3,3) real
  AP(9)  = (1.0d0, 1.0d0)     ! A(4,3) = conjg(A(3,4))
  AP(10) = (5.0d0, 0.0d0)     ! A(4,4) real
  call packed_herm_1norm('L', n, AP, ANORM)
  call ZHPTRF('L', n, AP, IPIV, INFO)
  call begin_test('4x4_lower')
  call print_scalar('anorm', ANORM)
  call print_int('info_trf', INFO)
  call print_array('AP_factored', AP_r, 2*nn)
  call print_int_array('ipiv', IPIV, n)
  call ZHPCON('L', n, AP, IPIV, ANORM, RCOND, WORK, INFO)
  call print_scalar('rcond', RCOND)
  call print_int('info', INFO)
  call end_test()

  ! Test 12: Real diagonal with complex off-diagonal (upper)
  n = 3
  nn = n*(n+1)/2
  AP = (0.0d0, 0.0d0)
  AP(1) = (2.0d0, 0.0d0)    ! real diag
  AP(3) = (3.0d0, 0.0d0)    ! real diag
  AP(6) = (4.0d0, 0.0d0)    ! real diag
  ANORM = 4.0d0
  call ZHPTRF('U', n, AP, IPIV, INFO)
  call begin_test('real_diag_upper')
  call print_scalar('anorm', ANORM)
  call print_int('info_trf', INFO)
  call print_array('AP_factored', AP_r, 2*nn)
  call print_int_array('ipiv', IPIV, n)
  call ZHPCON('U', n, AP, IPIV, ANORM, RCOND, WORK, INFO)
  call print_scalar('rcond', RCOND)
  call print_int('info', INFO)
  call end_test()

contains

  subroutine packed_herm_1norm(uplo, n, AP, anorm)
    ! Compute 1-norm of complex Hermitian packed matrix
    ! Same formula as symmetric: sum of abs values in each column
    character, intent(in) :: uplo
    integer, intent(in) :: n
    complex*16, intent(in) :: AP(*)
    double precision, intent(out) :: anorm

    double precision :: colsums(n)
    integer :: i, j, k

    colsums = 0.0d0
    if (uplo .eq. 'U' .or. uplo .eq. 'u') then
      k = 1
      do j = 1, n
        do i = 1, j
          if (i .eq. j) then
            colsums(j) = colsums(j) + abs(AP(k))
          else
            colsums(i) = colsums(i) + abs(AP(k))
            colsums(j) = colsums(j) + abs(AP(k))
          end if
          k = k + 1
        end do
      end do
    else
      k = 1
      do j = 1, n
        do i = j, n
          if (i .eq. j) then
            colsums(j) = colsums(j) + abs(AP(k))
          else
            colsums(i) = colsums(i) + abs(AP(k))
            colsums(j) = colsums(j) + abs(AP(k))
          end if
          k = k + 1
        end do
      end do
    end if

    anorm = 0.0d0
    do j = 1, n
      if (colsums(j) > anorm) anorm = colsums(j)
    end do
  end subroutine

end program
