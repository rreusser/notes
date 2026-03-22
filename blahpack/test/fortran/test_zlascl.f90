program test_zlascl
  use test_utils
  implicit none
  complex*16 :: a(4, 4)
  double precision :: a_r(32)
  equivalence (a, a_r)
  complex*16 :: ab2(2, 4)
  double precision :: ab2_r(16)
  equivalence (ab2, ab2_r)
  complex*16 :: ab3(3, 4)
  double precision :: ab3_r(24)
  equivalence (ab3, ab3_r)
  integer :: info, m, n, lda

  lda = 4

  ! Test 1: basic scaling of general matrix, cfrom=1, cto=2 (multiply by 2)
  m = 2
  n = 2
  a = (0.0d0, 0.0d0)
  a(1,1) = (1.0d0, 2.0d0)
  a(2,1) = (3.0d0, 4.0d0)
  a(1,2) = (5.0d0, 6.0d0)
  a(2,2) = (7.0d0, 8.0d0)
  call zlascl('G', 0, 0, 1.0d0, 2.0d0, m, n, a, lda, info)
  call begin_test('zlascl_basic')
  call print_cmatrix('a', a_r, lda, m, n)
  call print_int('info', info)
  call end_test()

  ! Test 2: scaling by 0.5 (cfrom=2, cto=1)
  m = 2
  n = 2
  a = (0.0d0, 0.0d0)
  a(1,1) = (2.0d0, 4.0d0)
  a(2,1) = (6.0d0, 8.0d0)
  a(1,2) = (10.0d0, 12.0d0)
  a(2,2) = (14.0d0, 16.0d0)
  call zlascl('G', 0, 0, 2.0d0, 1.0d0, m, n, a, lda, info)
  call begin_test('zlascl_half')
  call print_cmatrix('a', a_r, lda, m, n)
  call print_int('info', info)
  call end_test()

  ! Test 3: M=0 or N=0 quick return
  a = (0.0d0, 0.0d0)
  a(1,1) = (99.0d0, 88.0d0)
  call zlascl('G', 0, 0, 1.0d0, 2.0d0, 0, 2, a, lda, info)
  call begin_test('zlascl_m_zero')
  call print_cmatrix('a', a_r, lda, 1, 1)
  call print_int('info', info)
  call end_test()

  ! Test 4: upper triangular
  m = 3
  n = 3
  a = (0.0d0, 0.0d0)
  a(1,1) = (1.0d0, 0.0d0)
  a(1,2) = (2.0d0, 0.0d0)
  a(1,3) = (3.0d0, 0.0d0)
  a(2,2) = (4.0d0, 0.0d0)
  a(2,3) = (5.0d0, 0.0d0)
  a(3,3) = (6.0d0, 0.0d0)
  call zlascl('U', 0, 0, 1.0d0, 3.0d0, m, n, a, lda, info)
  call begin_test('zlascl_upper')
  call print_cmatrix('a', a_r, lda, m, n)
  call print_int('info', info)
  call end_test()

  ! Test 5: lower triangular
  m = 3
  n = 3
  a = (0.0d0, 0.0d0)
  a(1,1) = (1.0d0, 0.0d0)
  a(2,1) = (2.0d0, 0.0d0)
  a(3,1) = (3.0d0, 0.0d0)
  a(2,2) = (4.0d0, 0.0d0)
  a(3,2) = (5.0d0, 0.0d0)
  a(3,3) = (6.0d0, 0.0d0)
  call zlascl('L', 0, 0, 1.0d0, 3.0d0, m, n, a, lda, info)
  call begin_test('zlascl_lower')
  call print_cmatrix('a', a_r, lda, m, n)
  call print_int('info', info)
  call end_test()

  ! Test 6: cfrom=cto (should be identity, MUL=1 quick return)
  m = 2
  n = 2
  a = (0.0d0, 0.0d0)
  a(1,1) = (1.0d0, 2.0d0)
  a(2,1) = (3.0d0, 4.0d0)
  a(1,2) = (5.0d0, 6.0d0)
  a(2,2) = (7.0d0, 8.0d0)
  call zlascl('G', 0, 0, 5.0d0, 5.0d0, m, n, a, lda, info)
  call begin_test('zlascl_identity')
  call print_cmatrix('a', a_r, lda, m, n)
  call print_int('info', info)
  call end_test()

  ! Test 7: upper Hessenberg
  m = 3
  n = 3
  a = (0.0d0, 0.0d0)
  a(1,1) = (1.0d0, 0.0d0)
  a(2,1) = (2.0d0, 0.0d0)
  a(1,2) = (3.0d0, 0.0d0)
  a(2,2) = (4.0d0, 0.0d0)
  a(3,2) = (5.0d0, 0.0d0)
  a(1,3) = (6.0d0, 0.0d0)
  a(2,3) = (7.0d0, 0.0d0)
  a(3,3) = (8.0d0, 0.0d0)
  call zlascl('H', 0, 0, 1.0d0, 2.0d0, m, n, a, lda, info)
  call begin_test('zlascl_hessenberg')
  call print_cmatrix('a', a_r, lda, m, n)
  call print_int('info', info)
  call end_test()

  ! Test 8: large cfrom/cto ratio to trigger iterative scaling while-loop
  ! cfrom=1e300, cto=1e-300 => ratio ~1e-600, requires multiple iterations
  m = 2
  n = 1
  a = (0.0d0, 0.0d0)
  a(1,1) = (1.0d0, 1.0d0)
  a(2,1) = (2.0d0, 3.0d0)
  call zlascl('G', 0, 0, 1.0d300, 1.0d-300, m, n, a, lda, info)
  call begin_test('zlascl_large_ratio')
  call print_cmatrix('a', a_r, lda, m, n)
  call print_int('info', info)
  call end_test()

  ! Test 9: large cto/cfrom ratio (triggers mul=bignum branch)
  ! cfrom=1e-150, cto=1e150 => ratio 1e300, triggers multiple iterations
  ! Use small input values to avoid infinity in results
  m = 2
  n = 1
  a = (0.0d0, 0.0d0)
  a(1,1) = (1.0d-150, 1.0d-150)
  a(2,1) = (2.0d-150, 3.0d-150)
  call zlascl('G', 0, 0, 1.0d-150, 1.0d150, m, n, a, lda, info)
  call begin_test('zlascl_large_ratio_inv')
  call print_cmatrix('a', a_r, lda, m, n)
  call print_int('info', info)
  call end_test()

  ! Test 10: lower band matrix (type='B'), kl=ku=1, M=N=3
  ! Band storage: kl+1=2 rows by N=3 columns
  ! For 'B', requires M==N and KL==KU
  ab2 = (0.0d0, 0.0d0)
  ! diagonal: row 1
  ab2(1,1) = (1.0d0, 0.0d0)
  ab2(1,2) = (2.0d0, 0.0d0)
  ab2(1,3) = (3.0d0, 0.0d0)
  ! sub-diagonal: row 2
  ab2(2,1) = (4.0d0, 0.0d0)
  ab2(2,2) = (5.0d0, 0.0d0)
  call zlascl('B', 1, 1, 1.0d0, 3.0d0, 3, 3, ab2, 2, info)
  call begin_test('zlascl_lower_band')
  call print_cmatrix_band('a', ab2_r, 2, 2, 3)
  call print_int('info', info)
  call end_test()

  ! Test 11: upper band matrix (type='Q'), kl=ku=1, M=N=3
  ! Band storage: ku+1=2 rows by N=3 columns
  ! For 'Q', requires M==N and KL==KU
  ab2 = (0.0d0, 0.0d0)
  ! super-diagonal: row 1
  ab2(1,2) = (1.0d0, 0.0d0)
  ab2(1,3) = (2.0d0, 0.0d0)
  ! diagonal: row 2
  ab2(2,1) = (3.0d0, 0.0d0)
  ab2(2,2) = (4.0d0, 0.0d0)
  ab2(2,3) = (5.0d0, 0.0d0)
  call zlascl('Q', 1, 1, 1.0d0, 3.0d0, 3, 3, ab2, 2, info)
  call begin_test('zlascl_upper_band')
  call print_cmatrix_band('a', ab2_r, 2, 2, 3)
  call print_int('info', info)
  call end_test()

  ! Test 12: full band matrix (type='Z'), kl=1, ku=1, M=N=3
  ! Band storage: 2*kl+ku+1=4 rows by N columns
  ! LDA must be >= 2*KL+KU+1 = 4
  ! Using a(4,4) since we need LDA=4
  a = (0.0d0, 0.0d0)
  ! row 1: unused for j=1 (ku padding)
  a(1,2) = (1.0d0, 0.0d0)
  a(1,3) = (2.0d0, 0.0d0)
  ! row 2: super-diagonal (ku)
  a(2,1) = (3.0d0, 0.0d0)
  a(2,2) = (4.0d0, 0.0d0)
  a(2,3) = (5.0d0, 0.0d0)
  ! row 3: diagonal (kl+1)
  a(3,1) = (6.0d0, 0.0d0)
  a(3,2) = (7.0d0, 0.0d0)
  a(3,3) = (8.0d0, 0.0d0)
  ! row 4: sub-diagonal
  a(4,1) = (9.0d0, 0.0d0)
  a(4,2) = (10.0d0, 0.0d0)
  call zlascl('Z', 1, 1, 1.0d0, 2.0d0, 3, 3, a, lda, info)
  call begin_test('zlascl_band')
  call print_cmatrix('a', a_r, lda, 4, 3)
  call print_int('info', info)
  call end_test()

contains
  subroutine print_cmatrix_band(name, arr, lda_val, m_val, n_val)
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
