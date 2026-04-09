program test_dggev
  implicit none

  integer, parameter :: dp = kind(1.0d0)
  integer :: n, lda, ldb, ldvl, ldvr, lwork, info, i, j
  real(dp), allocatable :: A(:,:), B(:,:), Ain(:,:), Bin(:,:)
  real(dp), allocatable :: alphar(:), alphai(:), beta(:)
  real(dp), allocatable :: VL(:,:), VR(:,:), work(:)
  character(len=64) :: label

  ! Test 1: 2x2 diagonal - eigenvalues only
  label = '2x2_diag_eig_only'
  n = 2; lda = 2; ldb = 2; ldvl = 1; ldvr = 1
  lwork = max(1, 8*n)
  allocate(A(lda,n), B(ldb,n), Ain(lda,n), Bin(ldb,n))
  allocate(alphar(n), alphai(n), beta(n))
  allocate(VL(1,1), VR(1,1), work(lwork))
  A = 0.0d0; B = 0.0d0
  A(1,1) = 2.0d0; A(2,2) = 3.0d0
  B(1,1) = 1.0d0; B(2,2) = 1.0d0
  Ain = A; Bin = B
  call dggev('N','N', n, A, lda, B, ldb, alphar, alphai, beta, &
             VL, ldvl, VR, ldvr, work, lwork, info)
  call write_result(label, n, lda, ldb, ldvl, ldvr, Ain, Bin, alphar, &
                    alphai, beta, VL, VR, info, 'N', 'N')
  deallocate(A, B, Ain, Bin, alphar, alphai, beta, VL, VR, work)

  ! Test 2: 2x2 with both eigenvectors
  label = '2x2_both_vectors'
  n = 2; lda = 2; ldb = 2; ldvl = 2; ldvr = 2
  lwork = max(1, 8*n)
  allocate(A(lda,n), B(ldb,n), Ain(lda,n), Bin(ldb,n))
  allocate(alphar(n), alphai(n), beta(n))
  allocate(VL(ldvl,n), VR(ldvr,n), work(lwork))
  A = 0.0d0; B = 0.0d0
  A(1,1) = 1.0d0; A(1,2) = 2.0d0
  A(2,1) = 0.0d0; A(2,2) = 3.0d0
  B(1,1) = 1.0d0; B(1,2) = 0.0d0
  B(2,1) = 0.0d0; B(2,2) = 1.0d0
  Ain = A; Bin = B
  call dggev('V','V', n, A, lda, B, ldb, alphar, alphai, beta, &
             VL, ldvl, VR, ldvr, work, lwork, info)
  call write_result(label, n, lda, ldb, ldvl, ldvr, Ain, Bin, alphar, &
                    alphai, beta, VL, VR, info, 'V', 'V')
  deallocate(A, B, Ain, Bin, alphar, alphai, beta, VL, VR, work)

  ! Test 3: 3x3 general with right eigenvectors only
  label = '3x3_right_only'
  n = 3; lda = 3; ldb = 3; ldvl = 1; ldvr = 3
  lwork = max(1, 8*n)
  allocate(A(lda,n), B(ldb,n), Ain(lda,n), Bin(ldb,n))
  allocate(alphar(n), alphai(n), beta(n))
  allocate(VL(1,1), VR(ldvr,n), work(lwork))
  A(1,1) = 1.0d0; A(1,2) = 2.0d0; A(1,3) = 3.0d0
  A(2,1) = 4.0d0; A(2,2) = 5.0d0; A(2,3) = 6.0d0
  A(3,1) = 7.0d0; A(3,2) = 8.0d0; A(3,3) = 10.0d0
  B(1,1) = 1.0d0; B(1,2) = 0.0d0; B(1,3) = 0.0d0
  B(2,1) = 0.0d0; B(2,2) = 2.0d0; B(2,3) = 0.0d0
  B(3,1) = 0.0d0; B(3,2) = 0.0d0; B(3,3) = 3.0d0
  Ain = A; Bin = B
  call dggev('N','V', n, A, lda, B, ldb, alphar, alphai, beta, &
             VL, ldvl, VR, ldvr, work, lwork, info)
  call write_result(label, n, lda, ldb, ldvl, ldvr, Ain, Bin, alphar, &
                    alphai, beta, VL, VR, info, 'N', 'V')
  deallocate(A, B, Ain, Bin, alphar, alphai, beta, VL, VR, work)

  ! Test 4: 3x3 with left eigenvectors only
  label = '3x3_left_only'
  n = 3; lda = 3; ldb = 3; ldvl = 3; ldvr = 1
  lwork = max(1, 8*n)
  allocate(A(lda,n), B(ldb,n), Ain(lda,n), Bin(ldb,n))
  allocate(alphar(n), alphai(n), beta(n))
  allocate(VL(ldvl,n), VR(1,1), work(lwork))
  A(1,1) = 3.0d0; A(1,2) = 1.0d0; A(1,3) = 0.0d0
  A(2,1) = 1.0d0; A(2,2) = 3.0d0; A(2,3) = 1.0d0
  A(3,1) = 0.0d0; A(3,2) = 1.0d0; A(3,3) = 3.0d0
  B(1,1) = 1.0d0; B(1,2) = 0.0d0; B(1,3) = 0.0d0
  B(2,1) = 0.0d0; B(2,2) = 1.0d0; B(2,3) = 0.0d0
  B(3,1) = 0.0d0; B(3,2) = 0.0d0; B(3,3) = 1.0d0
  Ain = A; Bin = B
  call dggev('V','N', n, A, lda, B, ldb, alphar, alphai, beta, &
             VL, ldvl, VR, ldvr, work, lwork, info)
  call write_result(label, n, lda, ldb, ldvl, ldvr, Ain, Bin, alphar, &
                    alphai, beta, VL, VR, info, 'V', 'N')
  deallocate(A, B, Ain, Bin, alphar, alphai, beta, VL, VR, work)

  ! Test 5: 4x4 with complex conjugate eigenvalue pairs
  label = '4x4_complex_both'
  n = 4; lda = 4; ldb = 4; ldvl = 4; ldvr = 4
  lwork = max(1, 8*n)
  allocate(A(lda,n), B(ldb,n), Ain(lda,n), Bin(ldb,n))
  allocate(alphar(n), alphai(n), beta(n))
  allocate(VL(ldvl,n), VR(ldvr,n), work(lwork))
  A(1,1) = 0.0d0; A(1,2) = 1.0d0; A(1,3) = 0.0d0; A(1,4) = 0.0d0
  A(2,1) = -1.0d0; A(2,2) = 0.0d0; A(2,3) = 0.0d0; A(2,4) = 0.0d0
  A(3,1) = 0.0d0; A(3,2) = 0.0d0; A(3,3) = 0.0d0; A(3,4) = 2.0d0
  A(4,1) = 0.0d0; A(4,2) = 0.0d0; A(4,3) = -2.0d0; A(4,4) = 0.0d0
  B(1,1) = 1.0d0; B(1,2) = 0.0d0; B(1,3) = 0.0d0; B(1,4) = 0.0d0
  B(2,1) = 0.0d0; B(2,2) = 1.0d0; B(2,3) = 0.0d0; B(2,4) = 0.0d0
  B(3,1) = 0.0d0; B(3,2) = 0.0d0; B(3,3) = 1.0d0; B(3,4) = 0.0d0
  B(4,1) = 0.0d0; B(4,2) = 0.0d0; B(4,3) = 0.0d0; B(4,4) = 1.0d0
  Ain = A; Bin = B
  call dggev('V','V', n, A, lda, B, ldb, alphar, alphai, beta, &
             VL, ldvl, VR, ldvr, work, lwork, info)
  call write_result(label, n, lda, ldb, ldvl, ldvr, Ain, Bin, alphar, &
                    alphai, beta, VL, VR, info, 'V', 'V')
  deallocate(A, B, Ain, Bin, alphar, alphai, beta, VL, VR, work)

  ! Test 6: 4x4 general nonsymmetric with both eigenvectors
  label = '4x4_general_both'
  n = 4; lda = 4; ldb = 4; ldvl = 4; ldvr = 4
  lwork = max(1, 8*n)
  allocate(A(lda,n), B(ldb,n), Ain(lda,n), Bin(ldb,n))
  allocate(alphar(n), alphai(n), beta(n))
  allocate(VL(ldvl,n), VR(ldvr,n), work(lwork))
  A(1,1) = 3.9d0; A(1,2) = 12.5d0; A(1,3) = -34.5d0; A(1,4) = -0.5d0
  A(2,1) = 4.3d0; A(2,2) = 21.5d0; A(2,3) = -47.5d0; A(2,4) = 7.5d0
  A(3,1) = 4.3d0; A(3,2) = 21.5d0; A(3,3) = -43.5d0; A(3,4) = 3.5d0
  A(4,1) = 4.4d0; A(4,2) = 26.0d0; A(4,3) = -46.0d0; A(4,4) = 6.0d0
  B(1,1) = 1.0d0; B(1,2) = 2.0d0; B(1,3) = -3.0d0; B(1,4) = 1.0d0
  B(2,1) = 1.0d0; B(2,2) = 3.0d0; B(2,3) = -5.0d0; B(2,4) = 4.0d0
  B(3,1) = 1.0d0; B(3,2) = 3.0d0; B(3,3) = -4.0d0; B(3,4) = 3.0d0
  B(4,1) = 1.0d0; B(4,2) = 3.0d0; B(4,3) = -4.0d0; B(4,4) = 4.0d0
  Ain = A; Bin = B
  call dggev('V','V', n, A, lda, B, ldb, alphar, alphai, beta, &
             VL, ldvl, VR, ldvr, work, lwork, info)
  call write_result(label, n, lda, ldb, ldvl, ldvr, Ain, Bin, alphar, &
                    alphai, beta, VL, VR, info, 'V', 'V')
  deallocate(A, B, Ain, Bin, alphar, alphai, beta, VL, VR, work)

  ! Test 7: 1x1 trivial case
  label = '1x1_trivial'
  n = 1; lda = 1; ldb = 1; ldvl = 1; ldvr = 1
  lwork = max(1, 8*n)
  allocate(A(lda,n), B(ldb,n), Ain(lda,n), Bin(ldb,n))
  allocate(alphar(n), alphai(n), beta(n))
  allocate(VL(ldvl,n), VR(ldvr,n), work(lwork))
  A(1,1) = 5.0d0
  B(1,1) = 2.0d0
  Ain = A; Bin = B
  call dggev('V','V', n, A, lda, B, ldb, alphar, alphai, beta, &
             VL, ldvl, VR, ldvr, work, lwork, info)
  call write_result(label, n, lda, ldb, ldvl, ldvr, Ain, Bin, alphar, &
                    alphai, beta, VL, VR, info, 'V', 'V')
  deallocate(A, B, Ain, Bin, alphar, alphai, beta, VL, VR, work)

contains

  subroutine write_result(label, n, lda, ldb, ldvl, ldvr, A, B, alphar, &
                          alphai, beta, VL, VR, info, jobvl, jobvr)
    character(len=*), intent(in) :: label, jobvl, jobvr
    integer, intent(in) :: n, lda, ldb, ldvl, ldvr, info
    real(dp), intent(in) :: A(lda,*), B(ldb,*), alphar(*), alphai(*), beta(*)
    real(dp), intent(in) :: VL(ldvl,*), VR(ldvr,*)
    integer :: i, j
    character(len=1024) :: line

    write(*,'(A)',advance='no') '{"name":"' // trim(label) // '"'
    write(*,'(A,I0)',advance='no') ',"n":', n
    write(*,'(A,I0)',advance='no') ',"info":', info
    write(*,'(A)',advance='no') ',"jobvl":"' // jobvl // '"'
    write(*,'(A)',advance='no') ',"jobvr":"' // jobvr // '"'

    ! Write input A (column-major flat)
    write(*,'(A)',advance='no') ',"A":['
    do j = 1, n
      do i = 1, n
        if (j > 1 .or. i > 1) write(*,'(A)',advance='no') ','
        write(line,'(ES25.17)') A(i,j)
        write(*,'(A)',advance='no') trim(adjustl(line))
      end do
    end do
    write(*,'(A)',advance='no') ']'

    ! Write input B (column-major flat)
    write(*,'(A)',advance='no') ',"B":['
    do j = 1, n
      do i = 1, n
        if (j > 1 .or. i > 1) write(*,'(A)',advance='no') ','
        write(line,'(ES25.17)') B(i,j)
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

    ! Write VL if computed (column-major flat)
    if (jobvl == 'V') then
      write(*,'(A)',advance='no') ',"VL":['
      do j = 1, n
        do i = 1, n
          if (j > 1 .or. i > 1) write(*,'(A)',advance='no') ','
          write(line,'(ES25.17)') VL(i,j)
          write(*,'(A)',advance='no') trim(adjustl(line))
        end do
      end do
      write(*,'(A)',advance='no') ']'
    end if

    ! Write VR if computed (column-major flat)
    if (jobvr == 'V') then
      write(*,'(A)',advance='no') ',"VR":['
      do j = 1, n
        do i = 1, n
          if (j > 1 .or. i > 1) write(*,'(A)',advance='no') ','
          write(line,'(ES25.17)') VR(i,j)
          write(*,'(A)',advance='no') trim(adjustl(line))
        end do
      end do
      write(*,'(A)',advance='no') ']'
    end if

    write(*,'(A)') '}'
  end subroutine

end program test_dggev
