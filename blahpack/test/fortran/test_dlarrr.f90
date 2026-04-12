program test_dlarrr
  implicit none
  integer, parameter :: maxn = 10
  double precision :: d(maxn), e(maxn)
  integer :: n, info, i

  ! -------------------------------------------------------
  ! Test 1: Well-conditioned tridiagonal matrix (should return 0)
  ! Simple 1-2-1 tridiagonal with moderate off-diagonal
  ! -------------------------------------------------------
  n = 5
  do i = 1, n
    d(i) = 4.0d0
  end do
  do i = 1, n-1
    e(i) = 1.0d0
  end do
  call dlarrr(n, d, e, info)
  write(*,'(A)', advance='no') '{"N":'; write(*,'(I1)', advance='no') n
  write(*,'(A)', advance='no') ',"INFO":'; write(*,'(I1)', advance='no') info
  write(*,'(A)', advance='no') ',"d":['
  do i = 1, n
    if (i > 1) write(*,'(A)', advance='no') ','
    write(*,'(ES23.16)', advance='no') d(i)
  end do
  write(*,'(A)', advance='no') '],"e":['
  do i = 1, n-1
    if (i > 1) write(*,'(A)', advance='no') ','
    write(*,'(ES23.16)', advance='no') e(i)
  end do
  write(*,'(A)') ']}'

  ! -------------------------------------------------------
  ! Test 2: n=1 quick return (should return 0)
  ! -------------------------------------------------------
  n = 1
  d(1) = 5.0d0
  call dlarrr(n, d, e, info)
  write(*,'(A)', advance='no') '{"N":'; write(*,'(I1)', advance='no') n
  write(*,'(A)', advance='no') ',"INFO":'; write(*,'(I1)', advance='no') info
  write(*,'(A)', advance='no') ',"d":['
  write(*,'(ES23.16)', advance='no') d(1)
  write(*,'(A)') '],"e":[]}'

  ! -------------------------------------------------------
  ! Test 3: n=0 quick return (should return 0)
  ! -------------------------------------------------------
  n = 0
  call dlarrr(n, d, e, info)
  write(*,'(A)', advance='no') '{"N":'; write(*,'(I1)', advance='no') n
  write(*,'(A)', advance='no') ',"INFO":'; write(*,'(I1)', advance='no') info
  write(*,'(A)') ',"d":[],"e":[]}'

  ! -------------------------------------------------------
  ! Test 4: Tiny diagonal element (should return 1)
  ! -------------------------------------------------------
  n = 3
  d(1) = 4.0d0
  d(2) = 1.0d-320
  d(3) = 4.0d0
  e(1) = 1.0d0
  e(2) = 1.0d0
  call dlarrr(n, d, e, info)
  write(*,'(A)', advance='no') '{"N":'; write(*,'(I1)', advance='no') n
  write(*,'(A)', advance='no') ',"INFO":'; write(*,'(I1)', advance='no') info
  write(*,'(A)', advance='no') ',"d":['
  do i = 1, n
    if (i > 1) write(*,'(A)', advance='no') ','
    write(*,'(ES23.16)', advance='no') d(i)
  end do
  write(*,'(A)', advance='no') '],"e":['
  do i = 1, n-1
    if (i > 1) write(*,'(A)', advance='no') ','
    write(*,'(ES23.16)', advance='no') e(i)
  end do
  write(*,'(A)') ']}'

  ! -------------------------------------------------------
  ! Test 5: Large off-diagonal relative to diagonal (return 1)
  ! offdig accumulation triggers RELCOND check
  ! -------------------------------------------------------
  n = 4
  d(1) = 1.0d0
  d(2) = 1.0d0
  d(3) = 1.0d0
  d(4) = 1.0d0
  e(1) = 0.99d0
  e(2) = 0.99d0
  e(3) = 0.99d0
  call dlarrr(n, d, e, info)
  write(*,'(A)', advance='no') '{"N":'; write(*,'(I1)', advance='no') n
  write(*,'(A)', advance='no') ',"INFO":'; write(*,'(I1)', advance='no') info
  write(*,'(A)', advance='no') ',"d":['
  do i = 1, n
    if (i > 1) write(*,'(A)', advance='no') ','
    write(*,'(ES23.16)', advance='no') d(i)
  end do
  write(*,'(A)', advance='no') '],"e":['
  do i = 1, n-1
    if (i > 1) write(*,'(A)', advance='no') ','
    write(*,'(ES23.16)', advance='no') e(i)
  end do
  write(*,'(A)') ']}'

  ! -------------------------------------------------------
  ! Test 6: Identity-like (should return 0)
  ! -------------------------------------------------------
  n = 4
  d(1) = 1.0d0
  d(2) = 1.0d0
  d(3) = 1.0d0
  d(4) = 1.0d0
  e(1) = 0.1d0
  e(2) = 0.1d0
  e(3) = 0.1d0
  call dlarrr(n, d, e, info)
  write(*,'(A)', advance='no') '{"N":'; write(*,'(I1)', advance='no') n
  write(*,'(A)', advance='no') ',"INFO":'; write(*,'(I1)', advance='no') info
  write(*,'(A)', advance='no') ',"d":['
  do i = 1, n
    if (i > 1) write(*,'(A)', advance='no') ','
    write(*,'(ES23.16)', advance='no') d(i)
  end do
  write(*,'(A)', advance='no') '],"e":['
  do i = 1, n-1
    if (i > 1) write(*,'(A)', advance='no') ','
    write(*,'(ES23.16)', advance='no') e(i)
  end do
  write(*,'(A)') ']}'

end program test_dlarrr
