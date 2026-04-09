program test_dgges
  implicit none

  integer, parameter :: dp = kind(1.0d0)
  integer :: n, lda, ldb, ldvsl, ldvsr, lwork, info, sdim, i, j
  real(dp), allocatable :: A(:,:), B(:,:), Ain(:,:), Bin(:,:)
  real(dp), allocatable :: alphar(:), alphai(:), beta(:)
  real(dp), allocatable :: VSL(:,:), VSR(:,:), work(:)
  logical, allocatable :: bwork(:)
  character(len=64) :: label

  ! Test 1: 2x2 diagonal - no vectors, no sorting
  label = '2x2_diag_no_vectors'
  n = 2; lda = 2; ldb = 2; ldvsl = 1; ldvsr = 1
  lwork = max(1, 8*n, 6*n+16)
  allocate(A(lda,n), B(ldb,n), Ain(lda,n), Bin(ldb,n))
  allocate(alphar(n), alphai(n), beta(n))
  allocate(VSL(1,1), VSR(1,1), work(lwork), bwork(n))
  A = 0.0d0; B = 0.0d0
  A(1,1) = 2.0d0; A(2,2) = 3.0d0
  B(1,1) = 1.0d0; B(2,2) = 1.0d0
  Ain = A; Bin = B
  call dgges('N','N','N', selctg_dummy, n, A, lda, B, ldb, sdim, &
             alphar, alphai, beta, VSL, ldvsl, VSR, ldvsr, &
             work, lwork, bwork, info)
  call write_result(label, n, lda, ldb, ldvsl, ldvsr, Ain, Bin, A, B, &
                    alphar, alphai, beta, VSL, VSR, info, sdim, 'N', 'N')
  deallocate(A, B, Ain, Bin, alphar, alphai, beta, VSL, VSR, work, bwork)

  ! Test 2: 2x2 with both Schur vectors
  label = '2x2_both_vectors'
  n = 2; lda = 2; ldb = 2; ldvsl = 2; ldvsr = 2
  lwork = max(1, 8*n, 6*n+16)
  allocate(A(lda,n), B(ldb,n), Ain(lda,n), Bin(ldb,n))
  allocate(alphar(n), alphai(n), beta(n))
  allocate(VSL(ldvsl,n), VSR(ldvsr,n), work(lwork), bwork(n))
  A = 0.0d0; B = 0.0d0
  A(1,1) = 1.0d0; A(1,2) = 2.0d0
  A(2,1) = 0.0d0; A(2,2) = 3.0d0
  B(1,1) = 1.0d0; B(1,2) = 0.0d0
  B(2,1) = 0.0d0; B(2,2) = 1.0d0
  Ain = A; Bin = B
  call dgges('V','V','N', selctg_dummy, n, A, lda, B, ldb, sdim, &
             alphar, alphai, beta, VSL, ldvsl, VSR, ldvsr, &
             work, lwork, bwork, info)
  call write_result(label, n, lda, ldb, ldvsl, ldvsr, Ain, Bin, A, B, &
                    alphar, alphai, beta, VSL, VSR, info, sdim, 'V', 'V')
  deallocate(A, B, Ain, Bin, alphar, alphai, beta, VSL, VSR, work, bwork)

  ! Test 3: 3x3 general with right Schur vectors only
  label = '3x3_right_only'
  n = 3; lda = 3; ldb = 3; ldvsl = 1; ldvsr = 3
  lwork = max(1, 8*n, 6*n+16)
  allocate(A(lda,n), B(ldb,n), Ain(lda,n), Bin(ldb,n))
  allocate(alphar(n), alphai(n), beta(n))
  allocate(VSL(1,1), VSR(ldvsr,n), work(lwork), bwork(n))
  A(1,1) = 1.0d0; A(1,2) = 2.0d0; A(1,3) = 3.0d0
  A(2,1) = 4.0d0; A(2,2) = 5.0d0; A(2,3) = 6.0d0
  A(3,1) = 7.0d0; A(3,2) = 8.0d0; A(3,3) = 10.0d0
  B(1,1) = 1.0d0; B(1,2) = 0.0d0; B(1,3) = 0.0d0
  B(2,1) = 0.0d0; B(2,2) = 2.0d0; B(2,3) = 0.0d0
  B(3,1) = 0.0d0; B(3,2) = 0.0d0; B(3,3) = 3.0d0
  Ain = A; Bin = B
  call dgges('N','V','N', selctg_dummy, n, A, lda, B, ldb, sdim, &
             alphar, alphai, beta, VSL, ldvsl, VSR, ldvsr, &
             work, lwork, bwork, info)
  call write_result(label, n, lda, ldb, ldvsl, ldvsr, Ain, Bin, A, B, &
                    alphar, alphai, beta, VSL, VSR, info, sdim, 'N', 'V')
  deallocate(A, B, Ain, Bin, alphar, alphai, beta, VSL, VSR, work, bwork)

  ! Test 4: 3x3 with left Schur vectors only
  label = '3x3_left_only'
  n = 3; lda = 3; ldb = 3; ldvsl = 3; ldvsr = 1
  lwork = max(1, 8*n, 6*n+16)
  allocate(A(lda,n), B(ldb,n), Ain(lda,n), Bin(ldb,n))
  allocate(alphar(n), alphai(n), beta(n))
  allocate(VSL(ldvsl,n), VSR(1,1), work(lwork), bwork(n))
  A(1,1) = 3.0d0; A(1,2) = 1.0d0; A(1,3) = 0.0d0
  A(2,1) = 1.0d0; A(2,2) = 3.0d0; A(2,3) = 1.0d0
  A(3,1) = 0.0d0; A(3,2) = 1.0d0; A(3,3) = 3.0d0
  B(1,1) = 1.0d0; B(1,2) = 0.0d0; B(1,3) = 0.0d0
  B(2,1) = 0.0d0; B(2,2) = 1.0d0; B(2,3) = 0.0d0
  B(3,1) = 0.0d0; B(3,2) = 0.0d0; B(3,3) = 1.0d0
  Ain = A; Bin = B
  call dgges('V','N','N', selctg_dummy, n, A, lda, B, ldb, sdim, &
             alphar, alphai, beta, VSL, ldvsl, VSR, ldvsr, &
             work, lwork, bwork, info)
  call write_result(label, n, lda, ldb, ldvsl, ldvsr, Ain, Bin, A, B, &
                    alphar, alphai, beta, VSL, VSR, info, sdim, 'V', 'N')
  deallocate(A, B, Ain, Bin, alphar, alphai, beta, VSL, VSR, work, bwork)

  ! Test 5: 4x4 with complex eigenvalues, both vectors
  label = '4x4_complex_eigs'
  n = 4; lda = 4; ldb = 4; ldvsl = 4; ldvsr = 4
  lwork = max(1, 8*n, 6*n+16)
  allocate(A(lda,n), B(ldb,n), Ain(lda,n), Bin(ldb,n))
  allocate(alphar(n), alphai(n), beta(n))
  allocate(VSL(ldvsl,n), VSR(ldvsr,n), work(lwork), bwork(n))
  A(1,1) = 0.0d0; A(1,2) = -1.0d0; A(1,3) = 0.0d0; A(1,4) = 0.0d0
  A(2,1) = 1.0d0; A(2,2) = 0.0d0;  A(2,3) = 0.0d0; A(2,4) = 0.0d0
  A(3,1) = 0.0d0; A(3,2) = 0.0d0;  A(3,3) = 0.0d0; A(3,4) = -2.0d0
  A(4,1) = 0.0d0; A(4,2) = 0.0d0;  A(4,3) = 2.0d0; A(4,4) = 0.0d0
  B(1,1) = 1.0d0; B(1,2) = 0.0d0; B(1,3) = 0.0d0; B(1,4) = 0.0d0
  B(2,1) = 0.0d0; B(2,2) = 1.0d0; B(2,3) = 0.0d0; B(2,4) = 0.0d0
  B(3,1) = 0.0d0; B(3,2) = 0.0d0; B(3,3) = 1.0d0; B(3,4) = 0.0d0
  B(4,1) = 0.0d0; B(4,2) = 0.0d0; B(4,3) = 0.0d0; B(4,4) = 1.0d0
  Ain = A; Bin = B
  call dgges('V','V','N', selctg_dummy, n, A, lda, B, ldb, sdim, &
             alphar, alphai, beta, VSL, ldvsl, VSR, ldvsr, &
             work, lwork, bwork, info)
  call write_result(label, n, lda, ldb, ldvsl, ldvsr, Ain, Bin, A, B, &
                    alphar, alphai, beta, VSL, VSR, info, sdim, 'V', 'V')
  deallocate(A, B, Ain, Bin, alphar, alphai, beta, VSL, VSR, work, bwork)

  ! Test 6: 1x1 trivial
  label = '1x1_trivial'
  n = 1; lda = 1; ldb = 1; ldvsl = 1; ldvsr = 1
  lwork = max(1, 8*n, 6*n+16)
  allocate(A(lda,n), B(ldb,n), Ain(lda,n), Bin(ldb,n))
  allocate(alphar(n), alphai(n), beta(n))
  allocate(VSL(ldvsl,n), VSR(ldvsr,n), work(lwork), bwork(n))
  A(1,1) = 5.0d0
  B(1,1) = 2.0d0
  Ain = A; Bin = B
  call dgges('V','V','N', selctg_dummy, n, A, lda, B, ldb, sdim, &
             alphar, alphai, beta, VSL, ldvsl, VSR, ldvsr, &
             work, lwork, bwork, info)
  call write_result(label, n, lda, ldb, ldvsl, ldvsr, Ain, Bin, A, B, &
                    alphar, alphai, beta, VSL, VSR, info, sdim, 'V', 'V')
  deallocate(A, B, Ain, Bin, alphar, alphai, beta, VSL, VSR, work, bwork)

  ! Test 7: 4x4 general nonsymmetric
  label = '4x4_general'
  n = 4; lda = 4; ldb = 4; ldvsl = 4; ldvsr = 4
  lwork = max(1, 8*n, 6*n+16)
  allocate(A(lda,n), B(ldb,n), Ain(lda,n), Bin(ldb,n))
  allocate(alphar(n), alphai(n), beta(n))
  allocate(VSL(ldvsl,n), VSR(ldvsr,n), work(lwork), bwork(n))
  A(1,1) = 3.9d0; A(1,2) = 12.5d0; A(1,3) = -34.5d0; A(1,4) = -0.5d0
  A(2,1) = 4.3d0; A(2,2) = 21.5d0; A(2,3) = -47.5d0; A(2,4) = 7.5d0
  A(3,1) = 4.3d0; A(3,2) = 21.5d0; A(3,3) = -43.5d0; A(3,4) = 3.5d0
  A(4,1) = 4.4d0; A(4,2) = 26.0d0; A(4,3) = -46.0d0; A(4,4) = 6.0d0
  B(1,1) = 1.0d0; B(1,2) = 2.0d0; B(1,3) = -3.0d0; B(1,4) = 1.0d0
  B(2,1) = 1.0d0; B(2,2) = 3.0d0; B(2,3) = -5.0d0; B(2,4) = 4.0d0
  B(3,1) = 1.0d0; B(3,2) = 3.0d0; B(3,3) = -4.0d0; B(3,4) = 3.0d0
  B(4,1) = 1.0d0; B(4,2) = 3.0d0; B(4,3) = -4.0d0; B(4,4) = 4.0d0
  Ain = A; Bin = B
  call dgges('V','V','N', selctg_dummy, n, A, lda, B, ldb, sdim, &
             alphar, alphai, beta, VSL, ldvsl, VSR, ldvsr, &
             work, lwork, bwork, info)
  call write_result(label, n, lda, ldb, ldvsl, ldvsr, Ain, Bin, A, B, &
                    alphar, alphai, beta, VSL, VSR, info, sdim, 'V', 'V')
  deallocate(A, B, Ain, Bin, alphar, alphai, beta, VSL, VSR, work, bwork)

contains

  logical function selctg_dummy(alphar, alphai, beta)
    real(dp), intent(in) :: alphar, alphai, beta
    selctg_dummy = .false.
  end function selctg_dummy

  subroutine write_result(label, n, lda, ldb, ldvsl, ldvsr, Ain, Bin, &
                          S, T, alphar, alphai, beta, VSL, VSR, info, &
                          sdim, jobvsl, jobvsr)
    character(len=*), intent(in) :: label, jobvsl, jobvsr
    integer, intent(in) :: n, lda, ldb, ldvsl, ldvsr, info, sdim
    real(dp), intent(in) :: Ain(lda,n), Bin(ldb,n)
    real(dp), intent(in) :: S(lda,n), T(ldb,n)
    real(dp), intent(in) :: alphar(n), alphai(n), beta(n)
    real(dp), intent(in) :: VSL(ldvsl,*), VSR(ldvsr,*)
    integer :: i, j
    character(len=64) :: line

    write(*,'(A)',advance='no') '{"name":"'
    write(*,'(A)',advance='no') trim(label)
    write(*,'(A)',advance='no') '"'

    ! Write scalar params
    write(*,'(A,I0)',advance='no') ',"n":', n
    write(*,'(A,I0)',advance='no') ',"info":', info
    write(*,'(A,I0)',advance='no') ',"sdim":', sdim
    write(*,'(A,A,A)',advance='no') ',"jobvsl":"', jobvsl, '"'
    write(*,'(A,A,A)',advance='no') ',"jobvsr":"', jobvsr, '"'

    ! Write input A (column-major flat)
    write(*,'(A)',advance='no') ',"Ain":['
    do j = 1, n
      do i = 1, n
        if (j > 1 .or. i > 1) write(*,'(A)',advance='no') ','
        write(line,'(ES25.17)') Ain(i,j)
        write(*,'(A)',advance='no') trim(adjustl(line))
      end do
    end do
    write(*,'(A)',advance='no') ']'

    ! Write input B (column-major flat)
    write(*,'(A)',advance='no') ',"Bin":['
    do j = 1, n
      do i = 1, n
        if (j > 1 .or. i > 1) write(*,'(A)',advance='no') ','
        write(line,'(ES25.17)') Bin(i,j)
        write(*,'(A)',advance='no') trim(adjustl(line))
      end do
    end do
    write(*,'(A)',advance='no') ']'

    ! Write output Schur form S (column-major flat)
    write(*,'(A)',advance='no') ',"S":['
    do j = 1, n
      do i = 1, n
        if (j > 1 .or. i > 1) write(*,'(A)',advance='no') ','
        write(line,'(ES25.17)') S(i,j)
        write(*,'(A)',advance='no') trim(adjustl(line))
      end do
    end do
    write(*,'(A)',advance='no') ']'

    ! Write output T (column-major flat)
    write(*,'(A)',advance='no') ',"T":['
    do j = 1, n
      do i = 1, n
        if (j > 1 .or. i > 1) write(*,'(A)',advance='no') ','
        write(line,'(ES25.17)') T(i,j)
        write(*,'(A)',advance='no') trim(adjustl(line))
      end do
    end do
    write(*,'(A)',advance='no') ']'

    ! Write alphar
    write(*,'(A)',advance='no') ',"alphar":['
    do i = 1, n
      if (i > 1) write(*,'(A)',advance='no') ','
      write(line,'(ES25.17)') alphar(i)
      write(*,'(A)',advance='no') trim(adjustl(line))
    end do
    write(*,'(A)',advance='no') ']'

    ! Write alphai
    write(*,'(A)',advance='no') ',"alphai":['
    do i = 1, n
      if (i > 1) write(*,'(A)',advance='no') ','
      write(line,'(ES25.17)') alphai(i)
      write(*,'(A)',advance='no') trim(adjustl(line))
    end do
    write(*,'(A)',advance='no') ']'

    ! Write beta
    write(*,'(A)',advance='no') ',"beta":['
    do i = 1, n
      if (i > 1) write(*,'(A)',advance='no') ','
      write(line,'(ES25.17)') beta(i)
      write(*,'(A)',advance='no') trim(adjustl(line))
    end do
    write(*,'(A)',advance='no') ']'

    ! Write VSL if computed (column-major flat)
    if (jobvsl == 'V') then
      write(*,'(A)',advance='no') ',"VSL":['
      do j = 1, n
        do i = 1, n
          if (j > 1 .or. i > 1) write(*,'(A)',advance='no') ','
          write(line,'(ES25.17)') VSL(i,j)
          write(*,'(A)',advance='no') trim(adjustl(line))
        end do
      end do
      write(*,'(A)',advance='no') ']'
    end if

    ! Write VSR if computed (column-major flat)
    if (jobvsr == 'V') then
      write(*,'(A)',advance='no') ',"VSR":['
      do j = 1, n
        do i = 1, n
          if (j > 1 .or. i > 1) write(*,'(A)',advance='no') ','
          write(line,'(ES25.17)') VSR(i,j)
          write(*,'(A)',advance='no') trim(adjustl(line))
        end do
      end do
      write(*,'(A)',advance='no') ']'
    end if

    write(*,'(A)') '}'
  end subroutine

end program test_dgges
