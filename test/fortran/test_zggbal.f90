program test_zggbal
  use test_utils
  implicit none
  integer, parameter :: MAXN = 6
  complex*16 :: a(MAXN, MAXN), b(MAXN, MAXN)
  double precision :: a_r(2*MAXN, MAXN), b_r(2*MAXN, MAXN)
  equivalence (a, a_r)
  equivalence (b, b_r)
  double precision :: lscale(MAXN), rscale(MAXN), work(6*MAXN)
  integer :: info, n, ilo, ihi

  ! ===== Test 1: JOB='N' — set ilo=1, ihi=n, scales = 1 =====
  n = 4
  a = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0)
  a(1,1) = (1.0d0, 2.0d0)
  a(1,2) = (3.0d0, 4.0d0)
  a(2,1) = (5.0d0, 6.0d0)
  a(2,2) = (7.0d0, 8.0d0)
  a(3,3) = (9.0d0, 0.0d0)
  a(4,4) = (1.0d0, 1.0d0)
  b(1,1) = (1.0d0, 0.0d0)
  b(2,2) = (1.0d0, 0.0d0)
  b(3,3) = (1.0d0, 0.0d0)
  b(4,4) = (1.0d0, 0.0d0)
  lscale = 0.0d0
  rscale = 0.0d0
  call zggbal('N', n, a, MAXN, b, MAXN, ilo, ihi, lscale, rscale, work, info)
  call begin_test('job_n')
  call print_int('info', info)
  call print_int('ilo', ilo)
  call print_int('ihi', ihi)
  call print_array('lscale', lscale, n)
  call print_array('rscale', rscale, n)
  call end_test()

  ! ===== Test 2: N=0 — quick return =====
  call zggbal('B', 0, a, MAXN, b, MAXN, ilo, ihi, lscale, rscale, work, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call print_int('ilo', ilo)
  call print_int('ihi', ihi)
  call end_test()

  ! ===== Test 3: N=1 — quick return with scales=1 =====
  n = 1
  a = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0)
  a(1,1) = (5.0d0, 3.0d0)
  b(1,1) = (1.0d0, 0.0d0)
  lscale = 0.0d0
  rscale = 0.0d0
  call zggbal('B', n, a, MAXN, b, MAXN, ilo, ihi, lscale, rscale, work, info)
  call begin_test('n_one')
  call print_int('info', info)
  call print_int('ilo', ilo)
  call print_int('ihi', ihi)
  call print_array('lscale', lscale, n)
  call print_array('rscale', rscale, n)
  call end_test()

  ! ===== Test 4: JOB='P' — permute only =====
  ! Use a 4x4 matrix with isolated eigenvalues:
  ! Row 4 of A and B has nonzero only in column 3 → isolate on bottom
  ! Column 1 has nonzero only in row 2 → isolate on top
  n = 4
  a = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0)
  ! Build a matrix where row 4 has single nonzero in columns 1:3
  a(1,1) = (1.0d0, 0.0d0)
  a(1,2) = (2.0d0, 0.0d0)
  a(2,2) = (3.0d0, 0.0d0)
  a(2,3) = (4.0d0, 0.0d0)
  a(3,2) = (5.0d0, 0.0d0)
  a(3,3) = (6.0d0, 0.0d0)
  a(4,3) = (7.0d0, 0.0d0)
  a(4,4) = (8.0d0, 0.0d0)
  b(1,1) = (1.0d0, 0.0d0)
  b(2,2) = (1.0d0, 0.0d0)
  b(3,3) = (1.0d0, 0.0d0)
  b(4,4) = (1.0d0, 0.0d0)
  lscale = 0.0d0
  rscale = 0.0d0
  call zggbal('P', n, a, MAXN, b, MAXN, ilo, ihi, lscale, rscale, work, info)
  call begin_test('job_p')
  call print_int('info', info)
  call print_int('ilo', ilo)
  call print_int('ihi', ihi)
  call print_array('lscale', lscale, n)
  call print_array('rscale', rscale, n)
  call print_cmatrix('a', a_r, MAXN, n, n)
  call print_cmatrix('b', b_r, MAXN, n, n)
  call end_test()

  ! ===== Test 5: JOB='S' — scale only =====
  ! Use a 3x3 pair with widely different magnitudes
  n = 3
  a = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0)
  a(1,1) = (1.0d3, 0.0d0)
  a(1,2) = (1.0d0, 0.0d0)
  a(1,3) = (1.0d-3, 0.0d0)
  a(2,1) = (1.0d0, 0.0d0)
  a(2,2) = (1.0d0, 0.0d0)
  a(2,3) = (1.0d0, 0.0d0)
  a(3,1) = (1.0d-3, 0.0d0)
  a(3,2) = (1.0d0, 0.0d0)
  a(3,3) = (1.0d3, 0.0d0)
  b(1,1) = (1.0d3, 0.0d0)
  b(1,2) = (1.0d0, 0.0d0)
  b(1,3) = (1.0d-3, 0.0d0)
  b(2,1) = (1.0d0, 0.0d0)
  b(2,2) = (1.0d0, 0.0d0)
  b(2,3) = (1.0d0, 0.0d0)
  b(3,1) = (1.0d-3, 0.0d0)
  b(3,2) = (1.0d0, 0.0d0)
  b(3,3) = (1.0d3, 0.0d0)
  lscale = 0.0d0
  rscale = 0.0d0
  call zggbal('S', n, a, MAXN, b, MAXN, ilo, ihi, lscale, rscale, work, info)
  call begin_test('job_s')
  call print_int('info', info)
  call print_int('ilo', ilo)
  call print_int('ihi', ihi)
  call print_array('lscale', lscale, n)
  call print_array('rscale', rscale, n)
  call print_cmatrix('a', a_r, MAXN, n, n)
  call print_cmatrix('b', b_r, MAXN, n, n)
  call end_test()

  ! ===== Test 6: JOB='B' — both permute and scale =====
  ! 4x4 complex pair: structured to exercise both phases
  n = 4
  a = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0)
  ! Dense 4x4 A with varying magnitude + imaginary parts
  a(1,1) = (1.0d0, 2.0d0)
  a(1,2) = (3.0d0, 1.0d0)
  a(1,3) = (0.1d0, 0.0d0)
  a(1,4) = (0.01d0, 0.0d0)
  a(2,1) = (4.0d0, 0.5d0)
  a(2,2) = (5.0d0, 3.0d0)
  a(2,3) = (2.0d0, 1.0d0)
  a(2,4) = (0.1d0, 0.0d0)
  a(3,1) = (0.1d0, 0.0d0)
  a(3,2) = (3.0d0, 2.0d0)
  a(3,3) = (6.0d0, 1.0d0)
  a(3,4) = (4.0d0, 0.5d0)
  a(4,1) = (0.01d0, 0.0d0)
  a(4,2) = (0.1d0, 0.0d0)
  a(4,3) = (3.0d0, 1.0d0)
  a(4,4) = (7.0d0, 2.0d0)
  b(1,1) = (1.0d0, 0.0d0)
  b(1,2) = (0.5d0, 0.0d0)
  b(1,3) = (0.01d0, 0.0d0)
  b(1,4) = (0.0d0, 0.0d0)
  b(2,1) = (0.5d0, 0.0d0)
  b(2,2) = (1.0d0, 0.0d0)
  b(2,3) = (0.5d0, 0.0d0)
  b(2,4) = (0.01d0, 0.0d0)
  b(3,1) = (0.01d0, 0.0d0)
  b(3,2) = (0.5d0, 0.0d0)
  b(3,3) = (1.0d0, 0.0d0)
  b(3,4) = (0.5d0, 0.0d0)
  b(4,1) = (0.0d0, 0.0d0)
  b(4,2) = (0.01d0, 0.0d0)
  b(4,3) = (0.5d0, 0.0d0)
  b(4,4) = (1.0d0, 0.0d0)
  lscale = 0.0d0
  rscale = 0.0d0
  call zggbal('B', n, a, MAXN, b, MAXN, ilo, ihi, lscale, rscale, work, info)
  call begin_test('job_b')
  call print_int('info', info)
  call print_int('ilo', ilo)
  call print_int('ihi', ihi)
  call print_array('lscale', lscale, n)
  call print_array('rscale', rscale, n)
  call print_cmatrix('a', a_r, MAXN, n, n)
  call print_cmatrix('b', b_r, MAXN, n, n)
  call end_test()

  ! ===== Test 7: JOB='P' with a matrix that has isolated eigenvalues =====
  ! Specifically designed: row 3 has single nonzero in col 3 (diagonal only)
  ! This should isolate it on the bottom
  n = 3
  a = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0)
  a(1,1) = (1.0d0, 0.0d0)
  a(1,2) = (2.0d0, 0.0d0)
  a(2,1) = (3.0d0, 0.0d0)
  a(2,2) = (4.0d0, 0.0d0)
  a(3,3) = (5.0d0, 0.0d0)
  b(1,1) = (1.0d0, 0.0d0)
  b(1,2) = (0.5d0, 0.0d0)
  b(2,1) = (0.5d0, 0.0d0)
  b(2,2) = (1.0d0, 0.0d0)
  b(3,3) = (1.0d0, 0.0d0)
  lscale = 0.0d0
  rscale = 0.0d0
  call zggbal('P', n, a, MAXN, b, MAXN, ilo, ihi, lscale, rscale, work, info)
  call begin_test('job_p_isolated')
  call print_int('info', info)
  call print_int('ilo', ilo)
  call print_int('ihi', ihi)
  call print_array('lscale', lscale, n)
  call print_array('rscale', rscale, n)
  call print_cmatrix('a', a_r, MAXN, n, n)
  call print_cmatrix('b', b_r, MAXN, n, n)
  call end_test()

  ! ===== Test 8: JOB='B' with complex entries and larger matrix =====
  ! 5x5 case to exercise scaling iteration more thoroughly
  n = 5
  a = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0)
  a(1,1) = (1.0d2, 1.0d0)
  a(1,2) = (1.0d0, 0.5d0)
  a(1,3) = (1.0d-2, 0.0d0)
  a(1,4) = (0.0d0, 0.0d0)
  a(1,5) = (0.0d0, 0.0d0)
  a(2,1) = (1.0d0, 0.0d0)
  a(2,2) = (1.0d2, 2.0d0)
  a(2,3) = (1.0d0, 0.5d0)
  a(2,4) = (1.0d-2, 0.0d0)
  a(2,5) = (0.0d0, 0.0d0)
  a(3,1) = (1.0d-2, 0.0d0)
  a(3,2) = (1.0d0, 0.0d0)
  a(3,3) = (1.0d2, 3.0d0)
  a(3,4) = (1.0d0, 0.5d0)
  a(3,5) = (1.0d-2, 0.0d0)
  a(4,1) = (0.0d0, 0.0d0)
  a(4,2) = (1.0d-2, 0.0d0)
  a(4,3) = (1.0d0, 0.0d0)
  a(4,4) = (1.0d2, 4.0d0)
  a(4,5) = (1.0d0, 0.5d0)
  a(5,1) = (0.0d0, 0.0d0)
  a(5,2) = (0.0d0, 0.0d0)
  a(5,3) = (1.0d-2, 0.0d0)
  a(5,4) = (1.0d0, 0.0d0)
  a(5,5) = (1.0d2, 5.0d0)
  b(1,1) = (1.0d2, 0.0d0)
  b(2,2) = (1.0d2, 0.0d0)
  b(3,3) = (1.0d2, 0.0d0)
  b(4,4) = (1.0d2, 0.0d0)
  b(5,5) = (1.0d2, 0.0d0)
  lscale = 0.0d0
  rscale = 0.0d0
  call zggbal('B', n, a, MAXN, b, MAXN, ilo, ihi, lscale, rscale, work, info)
  call begin_test('job_b_5x5')
  call print_int('info', info)
  call print_int('ilo', ilo)
  call print_int('ihi', ihi)
  call print_array('lscale', lscale, n)
  call print_array('rscale', rscale, n)
  call print_cmatrix('a', a_r, MAXN, n, n)
  call print_cmatrix('b', b_r, MAXN, n, n)
  call end_test()

  ! ===== Test 9: JOB='P' — fully diagonal matrix (everything isolated) =====
  n = 3
  a = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0)
  a(1,1) = (1.0d0, 0.0d0)
  a(2,2) = (2.0d0, 0.0d0)
  a(3,3) = (3.0d0, 0.0d0)
  b(1,1) = (1.0d0, 0.0d0)
  b(2,2) = (1.0d0, 0.0d0)
  b(3,3) = (1.0d0, 0.0d0)
  lscale = 0.0d0
  rscale = 0.0d0
  call zggbal('P', n, a, MAXN, b, MAXN, ilo, ihi, lscale, rscale, work, info)
  call begin_test('job_p_diagonal')
  call print_int('info', info)
  call print_int('ilo', ilo)
  call print_int('ihi', ihi)
  call print_array('lscale', lscale, n)
  call print_array('rscale', rscale, n)
  call end_test()

  ! ===== Test 10: JOB='S' — ilo=ihi (quick return from scaling) =====
  ! 2x2 diagonal — permutation isolates everything so ilo > ihi or ilo=ihi
  n = 2
  a = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0)
  a(1,1) = (1.0d0, 0.0d0)
  a(2,2) = (2.0d0, 0.0d0)
  b(1,1) = (1.0d0, 0.0d0)
  b(2,2) = (1.0d0, 0.0d0)
  lscale = 0.0d0
  rscale = 0.0d0
  call zggbal('S', n, a, MAXN, b, MAXN, ilo, ihi, lscale, rscale, work, info)
  call begin_test('job_s_trivial')
  call print_int('info', info)
  call print_int('ilo', ilo)
  call print_int('ihi', ihi)
  call print_array('lscale', lscale, n)
  call print_array('rscale', rscale, n)
  call end_test()

  ! ===== Test 11: JOB='B' with complex 2x2 dense =====
  n = 2
  a = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0)
  a(1,1) = (1.0d0, 1.0d0)
  a(1,2) = (2.0d0, 3.0d0)
  a(2,1) = (4.0d0, 5.0d0)
  a(2,2) = (6.0d0, 7.0d0)
  b(1,1) = (1.0d0, 0.0d0)
  b(1,2) = (0.5d0, 0.0d0)
  b(2,1) = (0.5d0, 0.0d0)
  b(2,2) = (1.0d0, 0.0d0)
  lscale = 0.0d0
  rscale = 0.0d0
  call zggbal('B', n, a, MAXN, b, MAXN, ilo, ihi, lscale, rscale, work, info)
  call begin_test('job_b_2x2')
  call print_int('info', info)
  call print_int('ilo', ilo)
  call print_int('ihi', ihi)
  call print_array('lscale', lscale, n)
  call print_array('rscale', rscale, n)
  call print_cmatrix('a', a_r, MAXN, n, n)
  call print_cmatrix('b', b_r, MAXN, n, n)
  call end_test()

  ! ===== Test 12: JOB='P', 5x5 matrix with two isolated rows/cols =====
  ! Row 5 has nonzero only in col 5 → isolate on bottom
  ! Col 1 has nonzero only in row 1 → isolate on left
  n = 5
  a = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0)
  a(1,1) = (1.0d0, 0.0d0)
  a(2,2) = (2.0d0, 1.0d0)
  a(2,3) = (3.0d0, 0.0d0)
  a(2,4) = (1.0d0, 0.0d0)
  a(3,2) = (4.0d0, 0.0d0)
  a(3,3) = (5.0d0, 2.0d0)
  a(3,4) = (2.0d0, 0.0d0)
  a(4,2) = (1.0d0, 0.0d0)
  a(4,3) = (3.0d0, 0.0d0)
  a(4,4) = (6.0d0, 1.0d0)
  a(5,5) = (7.0d0, 0.0d0)
  b(1,1) = (1.0d0, 0.0d0)
  b(2,2) = (1.0d0, 0.0d0)
  b(3,3) = (1.0d0, 0.0d0)
  b(4,4) = (1.0d0, 0.0d0)
  b(5,5) = (1.0d0, 0.0d0)
  lscale = 0.0d0
  rscale = 0.0d0
  call zggbal('P', n, a, MAXN, b, MAXN, ilo, ihi, lscale, rscale, work, info)
  call begin_test('job_p_5x5')
  call print_int('info', info)
  call print_int('ilo', ilo)
  call print_int('ihi', ihi)
  call print_array('lscale', lscale, n)
  call print_array('rscale', rscale, n)
  call print_cmatrix('a', a_r, MAXN, n, n)
  call print_cmatrix('b', b_r, MAXN, n, n)
  call end_test()

  ! ===== Test 13: JOB='B', fully dense 3x3 with complex entries =====
  ! All entries nonzero, complex — no permutation, just scaling
  n = 3
  a = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0)
  a(1,1) = (1.0d0, 1.0d0)
  a(1,2) = (2.0d0, 3.0d0)
  a(1,3) = (4.0d0, 5.0d0)
  a(2,1) = (6.0d0, 7.0d0)
  a(2,2) = (8.0d0, 9.0d0)
  a(2,3) = (10.0d0, 11.0d0)
  a(3,1) = (12.0d0, 13.0d0)
  a(3,2) = (14.0d0, 15.0d0)
  a(3,3) = (16.0d0, 17.0d0)
  b(1,1) = (1.0d0, 0.5d0)
  b(1,2) = (2.0d0, 1.0d0)
  b(1,3) = (3.0d0, 1.5d0)
  b(2,1) = (4.0d0, 2.0d0)
  b(2,2) = (5.0d0, 2.5d0)
  b(2,3) = (6.0d0, 3.0d0)
  b(3,1) = (7.0d0, 3.5d0)
  b(3,2) = (8.0d0, 4.0d0)
  b(3,3) = (9.0d0, 4.5d0)
  lscale = 0.0d0
  rscale = 0.0d0
  call zggbal('B', n, a, MAXN, b, MAXN, ilo, ihi, lscale, rscale, work, info)
  call begin_test('job_b_dense')
  call print_int('info', info)
  call print_int('ilo', ilo)
  call print_int('ihi', ihi)
  call print_array('lscale', lscale, n)
  call print_array('rscale', rscale, n)
  call print_cmatrix('a', a_r, MAXN, n, n)
  call print_cmatrix('b', b_r, MAXN, n, n)
  call end_test()

contains
  subroutine print_cmatrix(name, arr, lda_val, m_val, n_val)
    character(*), intent(in) :: name
    integer, intent(in) :: lda_val, m_val, n_val
    double precision, intent(in) :: arr(2*lda_val, *)
    integer :: i, j
    logical :: first
    write(*, '(A,A,A)', advance='no') ',"', trim(name), '":['
    first = .true.
    do j = 1, n_val
      do i = 1, m_val
        if (.not. first) write(*, '(A)', advance='no') ','
        first = .false.
        write(*, '(ES25.17E3)', advance='no') arr(2*(i-1)+1, j)
        write(*, '(A)', advance='no') ','
        write(*, '(ES25.17E3)', advance='no') arr(2*(i-1)+2, j)
      end do
    end do
    write(*, '(A)', advance='no') ']'
  end subroutine

end program
