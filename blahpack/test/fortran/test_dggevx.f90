program test_dggevx
  implicit none

  integer, parameter :: dp = kind(1.0d0)
  integer :: n, lda, ldb, ldvl, ldvr, lwork, info, ilo, ihi
  real(dp) :: abnrm, bbnrm
  real(dp), allocatable :: A(:,:), B(:,:), Ain(:,:), Bin(:,:)
  real(dp), allocatable :: alphar(:), alphai(:), beta(:)
  real(dp), allocatable :: VL(:,:), VR(:,:), work(:)
  real(dp), allocatable :: lscale(:), rscale(:), rconde(:), rcondv(:)
  integer, allocatable :: iwork(:)
  logical, allocatable :: bwork(:)
  character(len=64) :: label
  character(len=1) :: balanc, jobvl, jobvr, sense

  ! Test 1: 2x2 diagonal, no balancing, no vectors
  label = '2x2_diag_none'
  balanc = 'N'; jobvl = 'N'; jobvr = 'N'; sense = 'N'
  n = 2
  call setup(n, lda, ldb, ldvl, ldvr, lwork, jobvl, jobvr, sense)
  call alloc_all(n, lda, ldb, ldvl, ldvr, lwork, A, B, Ain, Bin, alphar, alphai, beta, VL, VR, work, lscale, rscale, rconde, rcondv, iwork, bwork)
  A = 0.0_dp; B = 0.0_dp
  A(1,1) = 2.0_dp; A(2,2) = 3.0_dp
  B(1,1) = 1.0_dp; B(2,2) = 1.0_dp
  Ain = A; Bin = B
  call dggevx(balanc, jobvl, jobvr, sense, n, A, lda, B, ldb, &
              alphar, alphai, beta, VL, ldvl, VR, ldvr, &
              ilo, ihi, lscale, rscale, abnrm, bbnrm, rconde, rcondv, &
              work, lwork, iwork, bwork, info)
  call write_result(label, n, lda, ldb, ldvl, ldvr, Ain, Bin, alphar, alphai, beta, VL, VR, lscale, rscale, rconde, rcondv, ilo, ihi, abnrm, bbnrm, info, balanc, jobvl, jobvr, sense)
  call dealloc_all(A, B, Ain, Bin, alphar, alphai, beta, VL, VR, work, lscale, rscale, rconde, rcondv, iwork, bwork)

  ! Test 2: 3x3 diagonal, permute balancing
  label = '3x3_diag_permute'
  balanc = 'P'; jobvl = 'N'; jobvr = 'N'; sense = 'N'
  n = 3
  call setup(n, lda, ldb, ldvl, ldvr, lwork, jobvl, jobvr, sense)
  call alloc_all(n, lda, ldb, ldvl, ldvr, lwork, A, B, Ain, Bin, alphar, alphai, beta, VL, VR, work, lscale, rscale, rconde, rcondv, iwork, bwork)
  A = 0.0_dp; B = 0.0_dp
  A(1,1) = 5.0_dp; A(2,2) = 6.0_dp; A(3,3) = 7.0_dp
  B(1,1) = 1.0_dp; B(2,2) = 2.0_dp; B(3,3) = 3.0_dp
  Ain = A; Bin = B
  call dggevx(balanc, jobvl, jobvr, sense, n, A, lda, B, ldb, &
              alphar, alphai, beta, VL, ldvl, VR, ldvr, &
              ilo, ihi, lscale, rscale, abnrm, bbnrm, rconde, rcondv, &
              work, lwork, iwork, bwork, info)
  call write_result(label, n, lda, ldb, ldvl, ldvr, Ain, Bin, alphar, alphai, beta, VL, VR, lscale, rscale, rconde, rcondv, ilo, ihi, abnrm, bbnrm, info, balanc, jobvl, jobvr, sense)
  call dealloc_all(A, B, Ain, Bin, alphar, alphai, beta, VL, VR, work, lscale, rscale, rconde, rcondv, iwork, bwork)

  ! Test 3: 3x3 upper triangular, both vectors, both balancing
  label = '3x3_triu_both'
  balanc = 'B'; jobvl = 'V'; jobvr = 'V'; sense = 'N'
  n = 3
  call setup(n, lda, ldb, ldvl, ldvr, lwork, jobvl, jobvr, sense)
  call alloc_all(n, lda, ldb, ldvl, ldvr, lwork, A, B, Ain, Bin, alphar, alphai, beta, VL, VR, work, lscale, rscale, rconde, rcondv, iwork, bwork)
  A = 0.0_dp; B = 0.0_dp
  A(1,1) = 1.0_dp; A(1,2) = 2.0_dp; A(1,3) = 3.0_dp
  A(2,2) = 4.0_dp; A(2,3) = 5.0_dp
  A(3,3) = 6.0_dp
  B(1,1) = 1.0_dp; B(2,2) = 1.0_dp; B(3,3) = 1.0_dp
  Ain = A; Bin = B
  call dggevx(balanc, jobvl, jobvr, sense, n, A, lda, B, ldb, &
              alphar, alphai, beta, VL, ldvl, VR, ldvr, &
              ilo, ihi, lscale, rscale, abnrm, bbnrm, rconde, rcondv, &
              work, lwork, iwork, bwork, info)
  call write_result(label, n, lda, ldb, ldvl, ldvr, Ain, Bin, alphar, alphai, beta, VL, VR, lscale, rscale, rconde, rcondv, ilo, ihi, abnrm, bbnrm, info, balanc, jobvl, jobvr, sense)
  call dealloc_all(A, B, Ain, Bin, alphar, alphai, beta, VL, VR, work, lscale, rscale, rconde, rcondv, iwork, bwork)

  ! Test 4: 4x4 complex conjugate eigenvalue pairs, both vectors, scale balancing
  label = '4x4_complex_scale'
  balanc = 'S'; jobvl = 'V'; jobvr = 'V'; sense = 'N'
  n = 4
  call setup(n, lda, ldb, ldvl, ldvr, lwork, jobvl, jobvr, sense)
  call alloc_all(n, lda, ldb, ldvl, ldvr, lwork, A, B, Ain, Bin, alphar, alphai, beta, VL, VR, work, lscale, rscale, rconde, rcondv, iwork, bwork)
  A = 0.0_dp; B = 0.0_dp
  A(1,1) = 0.0_dp; A(1,2) = 1.0_dp
  A(2,1) = -1.0_dp; A(2,2) = 0.0_dp
  A(3,3) = 0.0_dp; A(3,4) = 2.0_dp
  A(4,3) = -2.0_dp; A(4,4) = 0.0_dp
  B(1,1) = 1.0_dp; B(2,2) = 1.0_dp; B(3,3) = 1.0_dp; B(4,4) = 1.0_dp
  Ain = A; Bin = B
  call dggevx(balanc, jobvl, jobvr, sense, n, A, lda, B, ldb, &
              alphar, alphai, beta, VL, ldvl, VR, ldvr, &
              ilo, ihi, lscale, rscale, abnrm, bbnrm, rconde, rcondv, &
              work, lwork, iwork, bwork, info)
  call write_result(label, n, lda, ldb, ldvl, ldvr, Ain, Bin, alphar, alphai, beta, VL, VR, lscale, rscale, rconde, rcondv, ilo, ihi, abnrm, bbnrm, info, balanc, jobvl, jobvr, sense)
  call dealloc_all(A, B, Ain, Bin, alphar, alphai, beta, VL, VR, work, lscale, rscale, rconde, rcondv, iwork, bwork)

  ! Test 5: 4x4 general nonsymmetric, both vectors, both balancing
  label = '4x4_general_both'
  balanc = 'B'; jobvl = 'V'; jobvr = 'V'; sense = 'N'
  n = 4
  call setup(n, lda, ldb, ldvl, ldvr, lwork, jobvl, jobvr, sense)
  call alloc_all(n, lda, ldb, ldvl, ldvr, lwork, A, B, Ain, Bin, alphar, alphai, beta, VL, VR, work, lscale, rscale, rconde, rcondv, iwork, bwork)
  A(1,1) = 3.9_dp; A(1,2) = 12.5_dp; A(1,3) = -34.5_dp; A(1,4) = -0.5_dp
  A(2,1) = 4.3_dp; A(2,2) = 21.5_dp; A(2,3) = -47.5_dp; A(2,4) = 7.5_dp
  A(3,1) = 4.3_dp; A(3,2) = 21.5_dp; A(3,3) = -43.5_dp; A(3,4) = 3.5_dp
  A(4,1) = 4.4_dp; A(4,2) = 26.0_dp; A(4,3) = -46.0_dp; A(4,4) = 6.0_dp
  B(1,1) = 1.0_dp; B(1,2) = 2.0_dp; B(1,3) = -3.0_dp; B(1,4) = 1.0_dp
  B(2,1) = 1.0_dp; B(2,2) = 3.0_dp; B(2,3) = -5.0_dp; B(2,4) = 4.0_dp
  B(3,1) = 1.0_dp; B(3,2) = 3.0_dp; B(3,3) = -4.0_dp; B(3,4) = 3.0_dp
  B(4,1) = 1.0_dp; B(4,2) = 3.0_dp; B(4,3) = -4.0_dp; B(4,4) = 4.0_dp
  Ain = A; Bin = B
  call dggevx(balanc, jobvl, jobvr, sense, n, A, lda, B, ldb, &
              alphar, alphai, beta, VL, ldvl, VR, ldvr, &
              ilo, ihi, lscale, rscale, abnrm, bbnrm, rconde, rcondv, &
              work, lwork, iwork, bwork, info)
  call write_result(label, n, lda, ldb, ldvl, ldvr, Ain, Bin, alphar, alphai, beta, VL, VR, lscale, rscale, rconde, rcondv, ilo, ihi, abnrm, bbnrm, info, balanc, jobvl, jobvr, sense)
  call dealloc_all(A, B, Ain, Bin, alphar, alphai, beta, VL, VR, work, lscale, rscale, rconde, rcondv, iwork, bwork)

  ! Test 6: 1x1 trivial case with both balancing and both vectors
  label = '1x1_trivial'
  balanc = 'B'; jobvl = 'V'; jobvr = 'V'; sense = 'N'
  n = 1
  call setup(n, lda, ldb, ldvl, ldvr, lwork, jobvl, jobvr, sense)
  call alloc_all(n, lda, ldb, ldvl, ldvr, lwork, A, B, Ain, Bin, alphar, alphai, beta, VL, VR, work, lscale, rscale, rconde, rcondv, iwork, bwork)
  A(1,1) = 5.0_dp
  B(1,1) = 2.0_dp
  Ain = A; Bin = B
  call dggevx(balanc, jobvl, jobvr, sense, n, A, lda, B, ldb, &
              alphar, alphai, beta, VL, ldvl, VR, ldvr, &
              ilo, ihi, lscale, rscale, abnrm, bbnrm, rconde, rcondv, &
              work, lwork, iwork, bwork, info)
  call write_result(label, n, lda, ldb, ldvl, ldvr, Ain, Bin, alphar, alphai, beta, VL, VR, lscale, rscale, rconde, rcondv, ilo, ihi, abnrm, bbnrm, info, balanc, jobvl, jobvr, sense)
  call dealloc_all(A, B, Ain, Bin, alphar, alphai, beta, VL, VR, work, lscale, rscale, rconde, rcondv, iwork, bwork)

  ! Test 7: 4x4 general, sense = 'E' (eigenvalue condition numbers only)
  label = '4x4_sense_E'
  balanc = 'B'; jobvl = 'V'; jobvr = 'V'; sense = 'E'
  n = 4
  call setup(n, lda, ldb, ldvl, ldvr, lwork, jobvl, jobvr, sense)
  call alloc_all(n, lda, ldb, ldvl, ldvr, lwork, A, B, Ain, Bin, alphar, alphai, beta, VL, VR, work, lscale, rscale, rconde, rcondv, iwork, bwork)
  A(1,1) = 3.9_dp; A(1,2) = 12.5_dp; A(1,3) = -34.5_dp; A(1,4) = -0.5_dp
  A(2,1) = 4.3_dp; A(2,2) = 21.5_dp; A(2,3) = -47.5_dp; A(2,4) = 7.5_dp
  A(3,1) = 4.3_dp; A(3,2) = 21.5_dp; A(3,3) = -43.5_dp; A(3,4) = 3.5_dp
  A(4,1) = 4.4_dp; A(4,2) = 26.0_dp; A(4,3) = -46.0_dp; A(4,4) = 6.0_dp
  B(1,1) = 1.0_dp; B(1,2) = 2.0_dp; B(1,3) = -3.0_dp; B(1,4) = 1.0_dp
  B(2,1) = 1.0_dp; B(2,2) = 3.0_dp; B(2,3) = -5.0_dp; B(2,4) = 4.0_dp
  B(3,1) = 1.0_dp; B(3,2) = 3.0_dp; B(3,3) = -4.0_dp; B(3,4) = 3.0_dp
  B(4,1) = 1.0_dp; B(4,2) = 3.0_dp; B(4,3) = -4.0_dp; B(4,4) = 4.0_dp
  Ain = A; Bin = B
  call dggevx(balanc, jobvl, jobvr, sense, n, A, lda, B, ldb, &
              alphar, alphai, beta, VL, ldvl, VR, ldvr, &
              ilo, ihi, lscale, rscale, abnrm, bbnrm, rconde, rcondv, &
              work, lwork, iwork, bwork, info)
  call write_result(label, n, lda, ldb, ldvl, ldvr, Ain, Bin, alphar, alphai, beta, VL, VR, lscale, rscale, rconde, rcondv, ilo, ihi, abnrm, bbnrm, info, balanc, jobvl, jobvr, sense)
  call dealloc_all(A, B, Ain, Bin, alphar, alphai, beta, VL, VR, work, lscale, rscale, rconde, rcondv, iwork, bwork)

  ! Test 8: 4x4 general, sense = 'V' (right-eigenvector condition numbers)
  label = '4x4_sense_V'
  balanc = 'B'; jobvl = 'V'; jobvr = 'V'; sense = 'V'
  n = 4
  call setup(n, lda, ldb, ldvl, ldvr, lwork, jobvl, jobvr, sense)
  call alloc_all(n, lda, ldb, ldvl, ldvr, lwork, A, B, Ain, Bin, alphar, alphai, beta, VL, VR, work, lscale, rscale, rconde, rcondv, iwork, bwork)
  A(1,1) = 3.9_dp; A(1,2) = 12.5_dp; A(1,3) = -34.5_dp; A(1,4) = -0.5_dp
  A(2,1) = 4.3_dp; A(2,2) = 21.5_dp; A(2,3) = -47.5_dp; A(2,4) = 7.5_dp
  A(3,1) = 4.3_dp; A(3,2) = 21.5_dp; A(3,3) = -43.5_dp; A(3,4) = 3.5_dp
  A(4,1) = 4.4_dp; A(4,2) = 26.0_dp; A(4,3) = -46.0_dp; A(4,4) = 6.0_dp
  B(1,1) = 1.0_dp; B(1,2) = 2.0_dp; B(1,3) = -3.0_dp; B(1,4) = 1.0_dp
  B(2,1) = 1.0_dp; B(2,2) = 3.0_dp; B(2,3) = -5.0_dp; B(2,4) = 4.0_dp
  B(3,1) = 1.0_dp; B(3,2) = 3.0_dp; B(3,3) = -4.0_dp; B(3,4) = 3.0_dp
  B(4,1) = 1.0_dp; B(4,2) = 3.0_dp; B(4,3) = -4.0_dp; B(4,4) = 4.0_dp
  Ain = A; Bin = B
  call dggevx(balanc, jobvl, jobvr, sense, n, A, lda, B, ldb, &
              alphar, alphai, beta, VL, ldvl, VR, ldvr, &
              ilo, ihi, lscale, rscale, abnrm, bbnrm, rconde, rcondv, &
              work, lwork, iwork, bwork, info)
  call write_result(label, n, lda, ldb, ldvl, ldvr, Ain, Bin, alphar, alphai, beta, VL, VR, lscale, rscale, rconde, rcondv, ilo, ihi, abnrm, bbnrm, info, balanc, jobvl, jobvr, sense)
  call dealloc_all(A, B, Ain, Bin, alphar, alphai, beta, VL, VR, work, lscale, rscale, rconde, rcondv, iwork, bwork)

  ! Test 9: 4x4 general, sense = 'B' (both condition numbers)
  label = '4x4_sense_B'
  balanc = 'B'; jobvl = 'V'; jobvr = 'V'; sense = 'B'
  n = 4
  call setup(n, lda, ldb, ldvl, ldvr, lwork, jobvl, jobvr, sense)
  call alloc_all(n, lda, ldb, ldvl, ldvr, lwork, A, B, Ain, Bin, alphar, alphai, beta, VL, VR, work, lscale, rscale, rconde, rcondv, iwork, bwork)
  A(1,1) = 3.9_dp; A(1,2) = 12.5_dp; A(1,3) = -34.5_dp; A(1,4) = -0.5_dp
  A(2,1) = 4.3_dp; A(2,2) = 21.5_dp; A(2,3) = -47.5_dp; A(2,4) = 7.5_dp
  A(3,1) = 4.3_dp; A(3,2) = 21.5_dp; A(3,3) = -43.5_dp; A(3,4) = 3.5_dp
  A(4,1) = 4.4_dp; A(4,2) = 26.0_dp; A(4,3) = -46.0_dp; A(4,4) = 6.0_dp
  B(1,1) = 1.0_dp; B(1,2) = 2.0_dp; B(1,3) = -3.0_dp; B(1,4) = 1.0_dp
  B(2,1) = 1.0_dp; B(2,2) = 3.0_dp; B(2,3) = -5.0_dp; B(2,4) = 4.0_dp
  B(3,1) = 1.0_dp; B(3,2) = 3.0_dp; B(3,3) = -4.0_dp; B(3,4) = 3.0_dp
  B(4,1) = 1.0_dp; B(4,2) = 3.0_dp; B(4,3) = -4.0_dp; B(4,4) = 4.0_dp
  Ain = A; Bin = B
  call dggevx(balanc, jobvl, jobvr, sense, n, A, lda, B, ldb, &
              alphar, alphai, beta, VL, ldvl, VR, ldvr, &
              ilo, ihi, lscale, rscale, abnrm, bbnrm, rconde, rcondv, &
              work, lwork, iwork, bwork, info)
  call write_result(label, n, lda, ldb, ldvl, ldvr, Ain, Bin, alphar, alphai, beta, VL, VR, lscale, rscale, rconde, rcondv, ilo, ihi, abnrm, bbnrm, info, balanc, jobvl, jobvr, sense)
  call dealloc_all(A, B, Ain, Bin, alphar, alphai, beta, VL, VR, work, lscale, rscale, rconde, rcondv, iwork, bwork)

contains

  subroutine setup(n, lda, ldb, ldvl, ldvr, lwork, jobvl, jobvr, sense)
    integer, intent(in) :: n
    character(len=1), intent(in) :: jobvl, jobvr, sense
    integer, intent(out) :: lda, ldb, ldvl, ldvr, lwork
    lda = max(1, n); ldb = max(1, n)
    if (jobvl == 'V') then
      ldvl = max(1, n)
    else
      ldvl = 1
    end if
    if (jobvr == 'V') then
      ldvr = max(1, n)
    else
      ldvr = 1
    end if
    lwork = max(1, 6 * n * (n + 4) + 16)
    if (sense == 'N' .and. jobvl == 'N' .and. jobvr == 'N') then
      lwork = max(lwork, 2 * n)
    end if
  end subroutine

  subroutine alloc_all(n, lda, ldb, ldvl, ldvr, lwork, A, B, Ain, Bin, alphar, alphai, beta, VL, VR, work, lscale, rscale, rconde, rcondv, iwork, bwork)
    integer, intent(in) :: n, lda, ldb, ldvl, ldvr, lwork
    real(dp), allocatable, intent(out) :: A(:,:), B(:,:), Ain(:,:), Bin(:,:)
    real(dp), allocatable, intent(out) :: alphar(:), alphai(:), beta(:)
    real(dp), allocatable, intent(out) :: VL(:,:), VR(:,:), work(:)
    real(dp), allocatable, intent(out) :: lscale(:), rscale(:), rconde(:), rcondv(:)
    integer, allocatable, intent(out) :: iwork(:)
    logical, allocatable, intent(out) :: bwork(:)
    allocate(A(lda, n), B(ldb, n), Ain(lda, n), Bin(ldb, n))
    allocate(alphar(n), alphai(n), beta(n))
    allocate(VL(ldvl, n), VR(ldvr, n), work(lwork))
    allocate(lscale(n), rscale(n), rconde(n), rcondv(n))
    allocate(iwork(max(1, n + 6)), bwork(max(1, n)))
    A = 0.0_dp; B = 0.0_dp; Ain = 0.0_dp; Bin = 0.0_dp
    alphar = 0.0_dp; alphai = 0.0_dp; beta = 0.0_dp
    VL = 0.0_dp; VR = 0.0_dp; work = 0.0_dp
    lscale = 0.0_dp; rscale = 0.0_dp; rconde = 0.0_dp; rcondv = 0.0_dp
  end subroutine

  subroutine dealloc_all(A, B, Ain, Bin, alphar, alphai, beta, VL, VR, work, lscale, rscale, rconde, rcondv, iwork, bwork)
    real(dp), allocatable, intent(inout) :: A(:,:), B(:,:), Ain(:,:), Bin(:,:)
    real(dp), allocatable, intent(inout) :: alphar(:), alphai(:), beta(:)
    real(dp), allocatable, intent(inout) :: VL(:,:), VR(:,:), work(:)
    real(dp), allocatable, intent(inout) :: lscale(:), rscale(:), rconde(:), rcondv(:)
    integer, allocatable, intent(inout) :: iwork(:)
    logical, allocatable, intent(inout) :: bwork(:)
    deallocate(A, B, Ain, Bin, alphar, alphai, beta, VL, VR, work)
    deallocate(lscale, rscale, rconde, rcondv, iwork, bwork)
  end subroutine

  subroutine write_result(label, n, lda, ldb, ldvl, ldvr, A, B, alphar, alphai, beta, VL, VR, lscale, rscale, rconde, rcondv, ilo, ihi, abnrm, bbnrm, info, balanc, jobvl, jobvr, sense)
    character(len=*), intent(in) :: label
    character(len=1), intent(in) :: balanc, jobvl, jobvr, sense
    integer, intent(in) :: n, lda, ldb, ldvl, ldvr, info, ilo, ihi
    real(dp), intent(in) :: A(lda, *), B(ldb, *), alphar(*), alphai(*), beta(*)
    real(dp), intent(in) :: VL(ldvl, *), VR(ldvr, *)
    real(dp), intent(in) :: lscale(*), rscale(*), rconde(*), rcondv(*), abnrm, bbnrm
    integer :: i, j
    character(len=64) :: line

    write(*,'(A)',advance='no') '{"name":"' // trim(label) // '"'
    write(*,'(A,I0)',advance='no') ',"n":', n
    write(*,'(A,I0)',advance='no') ',"info":', info
    write(*,'(A,I0)',advance='no') ',"ilo":', ilo
    write(*,'(A,I0)',advance='no') ',"ihi":', ihi
    write(*,'(A)',advance='no') ',"balanc":"' // balanc // '"'
    write(*,'(A)',advance='no') ',"jobvl":"' // jobvl // '"'
    write(*,'(A)',advance='no') ',"jobvr":"' // jobvr // '"'
    write(*,'(A)',advance='no') ',"sense":"' // sense // '"'

    write(line,'(ES25.17)') abnrm
    write(*,'(A)',advance='no') ',"abnrm":' // trim(adjustl(line))
    write(line,'(ES25.17)') bbnrm
    write(*,'(A)',advance='no') ',"bbnrm":' // trim(adjustl(line))

    call write_mat('A', A, lda, n)
    call write_mat('B', B, ldb, n)
    call write_vec('alphar', alphar, n)
    call write_vec('alphai', alphai, n)
    call write_vec('beta', beta, n)
    call write_vec('lscale', lscale, n)
    call write_vec('rscale', rscale, n)

    if (jobvl == 'V') then
      call write_mat('VL', VL, ldvl, n)
    end if
    if (jobvr == 'V') then
      call write_mat('VR', VR, ldvr, n)
    end if

    if (sense == 'E' .or. sense == 'B') then
      call write_vec('rconde', rconde, n)
    end if
    if (sense == 'V' .or. sense == 'B') then
      call write_vec('rcondv', rcondv, n)
    end if

    write(*,'(A)') '}'
  end subroutine

  subroutine write_mat(name, M, ldm, n)
    character(len=*), intent(in) :: name
    integer, intent(in) :: ldm, n
    real(dp), intent(in) :: M(ldm, *)
    integer :: i, j
    character(len=64) :: line
    write(*,'(A)',advance='no') ',"' // trim(name) // '":['
    do j = 1, n
      do i = 1, n
        if (j > 1 .or. i > 1) write(*,'(A)',advance='no') ','
        write(line,'(ES25.17)') M(i, j)
        write(*,'(A)',advance='no') trim(adjustl(line))
      end do
    end do
    write(*,'(A)',advance='no') ']'
  end subroutine

  subroutine write_vec(name, V, n)
    character(len=*), intent(in) :: name
    integer, intent(in) :: n
    real(dp), intent(in) :: V(*)
    integer :: i
    character(len=64) :: line
    write(*,'(A)',advance='no') ',"' // trim(name) // '":['
    do i = 1, n
      if (i > 1) write(*,'(A)',advance='no') ','
      write(line,'(ES25.17)') V(i)
      write(*,'(A)',advance='no') trim(adjustl(line))
    end do
    write(*,'(A)',advance='no') ']'
  end subroutine

end program test_dggevx
