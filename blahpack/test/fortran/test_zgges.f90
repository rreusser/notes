program test_zgges
  implicit none

  integer, parameter :: dp = kind(1.0d0)
  integer, parameter :: NMAX = 4
  integer :: n, lda, ldb, ldvsl, ldvsr, lwork, info, sdim, i, j
  complex(dp) :: A(NMAX,NMAX), B(NMAX,NMAX), Ain(NMAX,NMAX), Bin(NMAX,NMAX)
  complex(dp) :: alpha(NMAX), beta(NMAX)
  complex(dp) :: VSL(NMAX,NMAX), VSR(NMAX,NMAX), work(200)
  double precision :: rwork(8*NMAX)
  logical :: bwork(NMAX)
  character(len=64) :: label

  ! For printing complex arrays as interleaved real/imag
  double precision :: A_r(2*NMAX*NMAX), B_r(2*NMAX*NMAX)
  double precision :: Ain_r(2*NMAX*NMAX), Bin_r(2*NMAX*NMAX)
  double precision :: alpha_r(2*NMAX), beta_r(2*NMAX)
  double precision :: VSL_r(2*NMAX*NMAX), VSR_r(2*NMAX*NMAX)

  lwork = 200

  ! Test 1: 2x2 diagonal - no vectors, no sorting
  label = '2x2_diag_no_vectors'
  n = 2; lda = NMAX; ldb = NMAX; ldvsl = NMAX; ldvsr = NMAX
  A = (0.0d0, 0.0d0); B = (0.0d0, 0.0d0)
  A(1,1) = (2.0d0, 0.0d0); A(2,2) = (3.0d0, 0.0d0)
  B(1,1) = (1.0d0, 0.0d0); B(2,2) = (1.0d0, 0.0d0)
  Ain = A; Bin = B
  call zgges('N','N','N', selctg_dummy, n, A, lda, B, ldb, sdim, &
             alpha, beta, VSL, ldvsl, VSR, ldvsr, &
             work, lwork, rwork, bwork, info)
  call pack_and_write(label, n, A, B, Ain, Bin, alpha, beta, VSL, VSR, &
                      info, sdim, 'N', 'N')

  ! Test 2: 2x2 with both Schur vectors
  label = '2x2_both_vectors'
  n = 2; lda = NMAX; ldb = NMAX; ldvsl = NMAX; ldvsr = NMAX
  A = (0.0d0, 0.0d0); B = (0.0d0, 0.0d0)
  A(1,1) = (1.0d0, 0.5d0); A(1,2) = (2.0d0, -1.0d0)
  A(2,1) = (0.0d0, 0.0d0); A(2,2) = (3.0d0, 0.5d0)
  B(1,1) = (1.0d0, 0.0d0); B(1,2) = (0.0d0, 0.0d0)
  B(2,1) = (0.0d0, 0.0d0); B(2,2) = (1.0d0, 0.0d0)
  Ain = A; Bin = B
  call zgges('V','V','N', selctg_dummy, n, A, lda, B, ldb, sdim, &
             alpha, beta, VSL, ldvsl, VSR, ldvsr, &
             work, lwork, rwork, bwork, info)
  call pack_and_write(label, n, A, B, Ain, Bin, alpha, beta, VSL, VSR, &
                      info, sdim, 'V', 'V')

  ! Test 3: 3x3 general with right Schur vectors only
  label = '3x3_right_only'
  n = 3; lda = NMAX; ldb = NMAX; ldvsl = NMAX; ldvsr = NMAX
  A = (0.0d0, 0.0d0); B = (0.0d0, 0.0d0)
  A(1,1) = (1.0d0, 1.0d0); A(1,2) = (2.0d0, 0.5d0); A(1,3) = (3.0d0, -1.0d0)
  A(2,1) = (4.0d0, -0.5d0); A(2,2) = (5.0d0, 1.0d0); A(2,3) = (6.0d0, 0.0d0)
  A(3,1) = (7.0d0, 0.0d0); A(3,2) = (8.0d0, -1.0d0); A(3,3) = (10.0d0, 2.0d0)
  B(1,1) = (1.0d0, 0.0d0); B(1,2) = (0.0d0, 0.0d0); B(1,3) = (0.0d0, 0.0d0)
  B(2,1) = (0.0d0, 0.0d0); B(2,2) = (2.0d0, 0.0d0); B(2,3) = (0.0d0, 0.0d0)
  B(3,1) = (0.0d0, 0.0d0); B(3,2) = (0.0d0, 0.0d0); B(3,3) = (3.0d0, 0.0d0)
  Ain = A; Bin = B
  call zgges('N','V','N', selctg_dummy, n, A, lda, B, ldb, sdim, &
             alpha, beta, VSL, ldvsl, VSR, ldvsr, &
             work, lwork, rwork, bwork, info)
  call pack_and_write(label, n, A, B, Ain, Bin, alpha, beta, VSL, VSR, &
                      info, sdim, 'N', 'V')

  ! Test 4: 3x3 with left Schur vectors only
  label = '3x3_left_only'
  n = 3; lda = NMAX; ldb = NMAX; ldvsl = NMAX; ldvsr = NMAX
  A = (0.0d0, 0.0d0); B = (0.0d0, 0.0d0)
  A(1,1) = (3.0d0, 0.5d0); A(1,2) = (1.0d0, 0.0d0); A(1,3) = (0.0d0, 0.0d0)
  A(2,1) = (1.0d0, -0.5d0); A(2,2) = (3.0d0, 0.0d0); A(2,3) = (1.0d0, 0.5d0)
  A(3,1) = (0.0d0, 0.0d0); A(3,2) = (1.0d0, -0.5d0); A(3,3) = (3.0d0, -0.5d0)
  B(1,1) = (1.0d0, 0.0d0); B(1,2) = (0.0d0, 0.0d0); B(1,3) = (0.0d0, 0.0d0)
  B(2,1) = (0.0d0, 0.0d0); B(2,2) = (1.0d0, 0.0d0); B(2,3) = (0.0d0, 0.0d0)
  B(3,1) = (0.0d0, 0.0d0); B(3,2) = (0.0d0, 0.0d0); B(3,3) = (1.0d0, 0.0d0)
  Ain = A; Bin = B
  call zgges('V','N','N', selctg_dummy, n, A, lda, B, ldb, sdim, &
             alpha, beta, VSL, ldvsl, VSR, ldvsr, &
             work, lwork, rwork, bwork, info)
  call pack_and_write(label, n, A, B, Ain, Bin, alpha, beta, VSL, VSR, &
                      info, sdim, 'V', 'N')

  ! Test 5: 4x4 with complex entries, both vectors
  label = '4x4_complex_both'
  n = 4; lda = NMAX; ldb = NMAX; ldvsl = NMAX; ldvsr = NMAX
  A(1,1) = (0.0d0, 1.0d0); A(1,2) = (-1.0d0, 0.0d0); A(1,3) = (0.0d0, 0.0d0); A(1,4) = (0.0d0, 0.0d0)
  A(2,1) = (1.0d0, 0.0d0); A(2,2) = (0.0d0, -1.0d0); A(2,3) = (0.0d0, 0.0d0); A(2,4) = (0.0d0, 0.0d0)
  A(3,1) = (0.0d0, 0.0d0); A(3,2) = (0.0d0, 0.0d0); A(3,3) = (2.0d0, 1.0d0); A(3,4) = (-2.0d0, 0.5d0)
  A(4,1) = (0.0d0, 0.0d0); A(4,2) = (0.0d0, 0.0d0); A(4,3) = (2.0d0, -0.5d0); A(4,4) = (1.0d0, 1.0d0)
  B(1,1) = (1.0d0, 0.0d0); B(1,2) = (0.0d0, 0.0d0); B(1,3) = (0.0d0, 0.0d0); B(1,4) = (0.0d0, 0.0d0)
  B(2,1) = (0.0d0, 0.0d0); B(2,2) = (1.0d0, 0.0d0); B(2,3) = (0.0d0, 0.0d0); B(2,4) = (0.0d0, 0.0d0)
  B(3,1) = (0.0d0, 0.0d0); B(3,2) = (0.0d0, 0.0d0); B(3,3) = (1.0d0, 0.0d0); B(3,4) = (0.0d0, 0.0d0)
  B(4,1) = (0.0d0, 0.0d0); B(4,2) = (0.0d0, 0.0d0); B(4,3) = (0.0d0, 0.0d0); B(4,4) = (1.0d0, 0.0d0)
  Ain = A; Bin = B
  call zgges('V','V','N', selctg_dummy, n, A, lda, B, ldb, sdim, &
             alpha, beta, VSL, ldvsl, VSR, ldvsr, &
             work, lwork, rwork, bwork, info)
  call pack_and_write(label, n, A, B, Ain, Bin, alpha, beta, VSL, VSR, &
                      info, sdim, 'V', 'V')

  ! Test 6: 1x1 trivial
  label = '1x1_trivial'
  n = 1; lda = NMAX; ldb = NMAX; ldvsl = NMAX; ldvsr = NMAX
  A = (0.0d0, 0.0d0); B = (0.0d0, 0.0d0)
  A(1,1) = (5.0d0, 2.0d0)
  B(1,1) = (2.0d0, 0.5d0)
  Ain = A; Bin = B
  call zgges('V','V','N', selctg_dummy, n, A, lda, B, ldb, sdim, &
             alpha, beta, VSL, ldvsl, VSR, ldvsr, &
             work, lwork, rwork, bwork, info)
  call pack_and_write(label, n, A, B, Ain, Bin, alpha, beta, VSL, VSR, &
                      info, sdim, 'V', 'V')

  ! Test 7: 4x4 general nonsymmetric complex
  label = '4x4_general'
  n = 4; lda = NMAX; ldb = NMAX; ldvsl = NMAX; ldvsr = NMAX
  A(1,1) = (3.9d0, 1.0d0); A(1,2) = (12.5d0, -0.5d0); A(1,3) = (-34.5d0, 2.0d0); A(1,4) = (-0.5d0, 0.0d0)
  A(2,1) = (4.3d0, 0.0d0); A(2,2) = (21.5d0, 1.5d0); A(2,3) = (-47.5d0, 0.0d0); A(2,4) = (7.5d0, -1.0d0)
  A(3,1) = (4.3d0, -0.5d0); A(3,2) = (21.5d0, 0.0d0); A(3,3) = (-43.5d0, 1.0d0); A(3,4) = (3.5d0, 0.5d0)
  A(4,1) = (4.4d0, 0.5d0); A(4,2) = (26.0d0, -1.0d0); A(4,3) = (-46.0d0, 0.0d0); A(4,4) = (6.0d0, 1.0d0)
  B(1,1) = (1.0d0, 0.0d0); B(1,2) = (2.0d0, 0.5d0); B(1,3) = (-3.0d0, 0.0d0); B(1,4) = (1.0d0, -0.5d0)
  B(2,1) = (1.0d0, 0.0d0); B(2,2) = (3.0d0, 0.0d0); B(2,3) = (-5.0d0, 0.5d0); B(2,4) = (4.0d0, 0.0d0)
  B(3,1) = (1.0d0, -0.5d0); B(3,2) = (3.0d0, 0.0d0); B(3,3) = (-4.0d0, 0.0d0); B(3,4) = (3.0d0, 0.5d0)
  B(4,1) = (1.0d0, 0.0d0); B(4,2) = (3.0d0, -0.5d0); B(4,3) = (-4.0d0, 0.5d0); B(4,4) = (4.0d0, 0.0d0)
  Ain = A; Bin = B
  call zgges('V','V','N', selctg_dummy, n, A, lda, B, ldb, sdim, &
             alpha, beta, VSL, ldvsl, VSR, ldvsr, &
             work, lwork, rwork, bwork, info)
  call pack_and_write(label, n, A, B, Ain, Bin, alpha, beta, VSL, VSR, &
                      info, sdim, 'V', 'V')

contains

  logical function selctg_dummy(alpha, beta)
    complex(dp), intent(in) :: alpha, beta
    selctg_dummy = .false.
  end function selctg_dummy

  subroutine pack_and_write(label, n, A, B, Ain, Bin, alpha, beta, VSL, VSR, &
                            info, sdim, jobvsl, jobvsr)
    character(len=*), intent(in) :: label, jobvsl, jobvsr
    integer, intent(in) :: n, info, sdim
    complex(dp), intent(in) :: A(NMAX,NMAX), B(NMAX,NMAX)
    complex(dp), intent(in) :: Ain(NMAX,NMAX), Bin(NMAX,NMAX)
    complex(dp), intent(in) :: alpha(NMAX), beta(NMAX)
    complex(dp), intent(in) :: VSL(NMAX,NMAX), VSR(NMAX,NMAX)

    double precision :: arr(2*NMAX*NMAX)
    integer :: i, j, k
    character(len=64) :: line

    write(*,'(A)',advance='no') '{"name":"'
    write(*,'(A)',advance='no') trim(label)
    write(*,'(A)',advance='no') '"'

    ! Scalars
    write(*,'(A,I0)',advance='no') ',"n":', n
    write(*,'(A,I0)',advance='no') ',"info":', info
    write(*,'(A,I0)',advance='no') ',"sdim":', sdim
    write(*,'(A,A,A)',advance='no') ',"jobvsl":"', jobvsl, '"'
    write(*,'(A,A,A)',advance='no') ',"jobvsr":"', jobvsr, '"'

    ! Ain (pack n x n from NMAX x NMAX)
    k = 0
    do j = 1, n
      do i = 1, n
        arr(k+1) = dble(Ain(i,j))
        arr(k+2) = dimag(Ain(i,j))
        k = k + 2
      end do
    end do
    call print_array('Ain', arr, 2*n*n)

    ! Bin
    k = 0
    do j = 1, n
      do i = 1, n
        arr(k+1) = dble(Bin(i,j))
        arr(k+2) = dimag(Bin(i,j))
        k = k + 2
      end do
    end do
    call print_array('Bin', arr, 2*n*n)

    ! S (output A)
    k = 0
    do j = 1, n
      do i = 1, n
        arr(k+1) = dble(A(i,j))
        arr(k+2) = dimag(A(i,j))
        k = k + 2
      end do
    end do
    call print_array('S', arr, 2*n*n)

    ! T (output B)
    k = 0
    do j = 1, n
      do i = 1, n
        arr(k+1) = dble(B(i,j))
        arr(k+2) = dimag(B(i,j))
        k = k + 2
      end do
    end do
    call print_array('T', arr, 2*n*n)

    ! alpha
    k = 0
    do i = 1, n
      arr(k+1) = dble(alpha(i))
      arr(k+2) = dimag(alpha(i))
      k = k + 2
    end do
    call print_array('alpha', arr, 2*n)

    ! beta
    k = 0
    do i = 1, n
      arr(k+1) = dble(beta(i))
      arr(k+2) = dimag(beta(i))
      k = k + 2
    end do
    call print_array('beta', arr, 2*n)

    ! VSL if computed
    if (jobvsl == 'V') then
      k = 0
      do j = 1, n
        do i = 1, n
          arr(k+1) = dble(VSL(i,j))
          arr(k+2) = dimag(VSL(i,j))
          k = k + 2
        end do
      end do
      call print_array('VSL', arr, 2*n*n)
    end if

    ! VSR if computed
    if (jobvsr == 'V') then
      k = 0
      do j = 1, n
        do i = 1, n
          arr(k+1) = dble(VSR(i,j))
          arr(k+2) = dimag(VSR(i,j))
          k = k + 2
        end do
      end do
      call print_array('VSR', arr, 2*n*n)
    end if

    write(*,'(A)') '}'
  end subroutine

  subroutine print_array(name, arr, len)
    character(len=*), intent(in) :: name
    integer, intent(in) :: len
    double precision, intent(in) :: arr(len)
    integer :: i
    character(len=64) :: line

    write(*,'(A,A,A)',advance='no') ',"', name, '":['
    do i = 1, len
      if (i > 1) write(*,'(A)',advance='no') ','
      write(line,'(ES25.17)') arr(i)
      write(*,'(A)',advance='no') trim(adjustl(line))
    end do
    write(*,'(A)',advance='no') ']'
  end subroutine

end program test_zgges
