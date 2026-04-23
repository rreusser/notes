program test_dorhr_col
  use test_utils
  implicit none
  integer, parameter :: lwmax = 1024
  double precision :: a(400), tau(20), t(40*40), d(40), work(lwmax)
  integer :: info, m, n, nb, lda, ldt, k

  ! ---------- Test 1: M=5, N=3, NB=2 ----------
  m = 5; n = 3; nb = 2; lda = m; ldt = nb
  t = 0.0d0; d = 0.0d0
  call make_q( m, n, a, lda, tau, work, lwmax )
  call begin_test('5x3_nb2')
  call print_array('q_in', a, m*n)
  call dorhr_col( m, n, nb, a, lda, t, ldt, d, info )
  call print_array('a', a, m*n)
  call print_array('t', t, ldt*n)
  call print_array('d', d, n)
  call print_int('info', info)
  call end_test()

  ! ---------- Test 2: M=6, N=4, NB=2 ----------
  m = 6; n = 4; nb = 2; lda = m; ldt = nb
  t = 0.0d0; d = 0.0d0
  call make_q( m, n, a, lda, tau, work, lwmax )
  call begin_test('6x4_nb2')
  call print_array('q_in', a, m*n)
  call dorhr_col( m, n, nb, a, lda, t, ldt, d, info )
  call print_array('a', a, m*n)
  call print_array('t', t, ldt*n)
  call print_array('d', d, n)
  call print_int('info', info)
  call end_test()

  ! ---------- Test 3: M=N=4, NB=4 (single block) ----------
  m = 4; n = 4; nb = 4; lda = m; ldt = nb
  t = 0.0d0; d = 0.0d0
  call make_q( m, n, a, lda, tau, work, lwmax )
  call begin_test('4x4_nb4')
  call print_array('q_in', a, m*n)
  call dorhr_col( m, n, nb, a, lda, t, ldt, d, info )
  call print_array('a', a, m*n)
  call print_array('t', t, ldt*n)
  call print_array('d', d, n)
  call print_int('info', info)
  call end_test()

  ! ---------- Test 4: M=8, N=5, NB=3 ----------
  m = 8; n = 5; nb = 3; lda = m; ldt = nb
  t = 0.0d0; d = 0.0d0
  call make_q( m, n, a, lda, tau, work, lwmax )
  call begin_test('8x5_nb3')
  call print_array('q_in', a, m*n)
  call dorhr_col( m, n, nb, a, lda, t, ldt, d, info )
  call print_array('a', a, m*n)
  call print_array('t', t, ldt*n)
  call print_array('d', d, n)
  call print_int('info', info)
  call end_test()

  ! ---------- Test 5: M=N=1 ----------
  m = 1; n = 1; nb = 1; lda = 1; ldt = 1
  a = 0.0d0; t = 0.0d0; d = 0.0d0
  a(1) = 1.0d0
  call begin_test('1x1')
  call print_array('q_in', a, 1)
  call dorhr_col( m, n, nb, a, lda, t, ldt, d, info )
  call print_array('a', a, 1)
  call print_array('t', t, 1)
  call print_array('d', d, 1)
  call print_int('info', info)
  call end_test()

  ! ---------- Test 6: M=0 N=0 (quick return) ----------
  m = 0; n = 0; nb = 1; lda = 1; ldt = 1
  call dorhr_col( m, n, nb, a, lda, t, ldt, d, info )
  call begin_test('m_zero')
  call print_int('info', info)
  call end_test()

  ! ---------- Test 7: M=7, N=5, NB=5 (NB >= N so single panel) ----------
  m = 7; n = 5; nb = 5; lda = m; ldt = nb
  t = 0.0d0; d = 0.0d0
  call make_q( m, n, a, lda, tau, work, lwmax )
  call begin_test('7x5_nb5')
  call print_array('q_in', a, m*n)
  call dorhr_col( m, n, nb, a, lda, t, ldt, d, info )
  call print_array('a', a, m*n)
  call print_array('t', t, ldt*n)
  call print_array('d', d, n)
  call print_int('info', info)
  call end_test()

  k = 0  ! silence unused warning if any
contains

  subroutine make_q( mm, nn, aa, ld, tt, ww, lw )
    integer, intent(in)  :: mm, nn, ld, lw
    double precision, intent(out)   :: aa(ld*nn), tt(nn), ww(lw)
    integer :: ii, jj, iinfo
    ! Build a deterministic tall-skinny matrix, then overwrite with its
    ! orthonormal Q factor via DGEQRF + DORGQR.
    do jj = 1, nn
      do ii = 1, mm
        aa((jj-1)*ld + ii) = sin( real(ii + 3*jj, 8) ) + 0.5d0 * cos( real(2*ii - jj, 8) )
      end do
    end do
    call dgeqrf( mm, nn, aa, ld, tt, ww, lw, iinfo )
    call dorgqr( mm, nn, nn, aa, ld, tt, ww, lw, iinfo )
  end subroutine make_q

end program
