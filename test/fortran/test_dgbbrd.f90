program test_dgbbrd
  use test_utils
  implicit none
  double precision, allocatable :: AB(:,:), Q(:,:), PT(:,:), C(:,:)
  double precision, allocatable :: D(:), E(:), WORK(:)
  integer :: info
  integer :: m, n, kl, ku, ldab, ncc

  ! ============================================================
  ! Test 1: 5x5 tridiagonal (kl=1, ku=1), vect='N'
  ! Full A:
  !   [4 -1  0  0  0]
  !   [-1 4 -1  0  0]
  !   [0 -1  4 -1  0]
  !   [0  0 -1  4 -1]
  !   [0  0  0 -1  4]
  ! Band layout (ldab = kl+ku+1 = 3): row 1 = super, row 2 = diag, row 3 = sub
  m = 5; n = 5; kl = 1; ku = 1; ldab = 3; ncc = 0
  allocate(AB(ldab, n))
  allocate(D(min(m,n)), E(min(m,n)-1), WORK(2*max(m,n)))
  allocate(Q(1,1), PT(1,1), C(1,1))
  AB = 0.0d0
  ! Col 1
  AB(2,1) = 4.0d0; AB(3,1) = -1.0d0
  ! Col 2
  AB(1,2) = -1.0d0; AB(2,2) = 4.0d0; AB(3,2) = -1.0d0
  ! Col 3
  AB(1,3) = -1.0d0; AB(2,3) = 4.0d0; AB(3,3) = -1.0d0
  ! Col 4
  AB(1,4) = -1.0d0; AB(2,4) = 4.0d0; AB(3,4) = -1.0d0
  ! Col 5
  AB(1,5) = -1.0d0; AB(2,5) = 4.0d0
  call DGBBRD('N', m, n, ncc, kl, ku, AB, ldab, D, E, Q, 1, PT, 1, C, 1, WORK, info)
  call begin_test('tri_5x5_N')
  call print_array('D', D, min(m,n))
  call print_array('E', E, min(m,n)-1)
  call print_matrix('AB', AB, ldab, ldab, n)
  call print_int('info', info)
  call end_test()
  deallocate(AB, D, E, WORK, Q, PT, C)

  ! ============================================================
  ! Test 2: 5x5 pentadiagonal (kl=2, ku=2), vect='B'
  m = 5; n = 5; kl = 2; ku = 2; ldab = 5; ncc = 0
  allocate(AB(ldab, n))
  allocate(D(min(m,n)), E(min(m,n)-1), WORK(2*max(m,n)))
  allocate(Q(m,m), PT(n,n), C(1,1))
  AB = 0.0d0
  ! Full A:
  ! [6 -2  1  0  0]
  ! [-2 6 -2  1  0]
  ! [1 -2  6 -2  1]
  ! [0  1 -2  6 -2]
  ! [0  0  1 -2  6]
  ! ldab = 5, ku+1 = 3 is diagonal row
  ! Col 1
  AB(3,1) = 6.0d0; AB(4,1) = -2.0d0; AB(5,1) = 1.0d0
  ! Col 2
  AB(2,2) = -2.0d0; AB(3,2) = 6.0d0; AB(4,2) = -2.0d0; AB(5,2) = 1.0d0
  ! Col 3
  AB(1,3) = 1.0d0; AB(2,3) = -2.0d0; AB(3,3) = 6.0d0; AB(4,3) = -2.0d0; AB(5,3) = 1.0d0
  ! Col 4
  AB(1,4) = 1.0d0; AB(2,4) = -2.0d0; AB(3,4) = 6.0d0; AB(4,4) = -2.0d0
  ! Col 5
  AB(1,5) = 1.0d0; AB(2,5) = -2.0d0; AB(3,5) = 6.0d0
  call DGBBRD('B', m, n, ncc, kl, ku, AB, ldab, D, E, Q, m, PT, n, C, 1, WORK, info)
  call begin_test('penta_5x5_B')
  call print_array('D', D, min(m,n))
  call print_array('E', E, min(m,n)-1)
  call print_matrix('Q', Q, m, m, m)
  call print_matrix('PT', PT, n, n, n)
  call print_int('info', info)
  call end_test()
  deallocate(AB, D, E, WORK, Q, PT, C)

  ! ============================================================
  ! Test 3: 6x4 tall (m>n) tridiagonal, vect='Q', ncc > 0
  m = 6; n = 4; kl = 1; ku = 1; ldab = 3; ncc = 2
  allocate(AB(ldab, n))
  allocate(D(min(m,n)), E(min(m,n)-1), WORK(2*max(m,n)))
  allocate(Q(m,m), PT(1,1), C(m,ncc))
  AB = 0.0d0
  ! Full A(6x4):
  ! [3 -1  0  0]
  ! [-1 3 -1  0]
  ! [0 -1  3 -1]
  ! [0  0 -1  3]
  ! [0  0  0 -1]
  ! [0  0  0  0]
  ! Col 1
  AB(2,1) = 3.0d0; AB(3,1) = -1.0d0
  ! Col 2
  AB(1,2) = -1.0d0; AB(2,2) = 3.0d0; AB(3,2) = -1.0d0
  ! Col 3
  AB(1,3) = -1.0d0; AB(2,3) = 3.0d0; AB(3,3) = -1.0d0
  ! Col 4
  AB(1,4) = -1.0d0; AB(2,4) = 3.0d0; AB(3,4) = -1.0d0
  ! C = [1 2; 3 4; 5 6; 7 8; 9 10; 11 12]
  C(1,1) = 1.0d0; C(1,2) = 2.0d0
  C(2,1) = 3.0d0; C(2,2) = 4.0d0
  C(3,1) = 5.0d0; C(3,2) = 6.0d0
  C(4,1) = 7.0d0; C(4,2) = 8.0d0
  C(5,1) = 9.0d0; C(5,2) = 10.0d0
  C(6,1) = 11.0d0; C(6,2) = 12.0d0
  call DGBBRD('Q', m, n, ncc, kl, ku, AB, ldab, D, E, Q, m, PT, 1, C, m, WORK, info)
  call begin_test('tall_6x4_Q')
  call print_array('D', D, min(m,n))
  call print_array('E', E, min(m,n)-1)
  call print_matrix('Q', Q, m, m, m)
  call print_matrix('C', C, m, m, ncc)
  call print_int('info', info)
  call end_test()
  deallocate(AB, D, E, WORK, Q, PT, C)

  ! ============================================================
  ! Test 4: 4x6 wide (m<n) bidiagonal (kl=0, ku=1), vect='P'
  m = 4; n = 6; kl = 0; ku = 1; ldab = 2; ncc = 0
  allocate(AB(ldab, n))
  allocate(D(min(m,n)), E(min(m,n)-1), WORK(2*max(m,n)))
  allocate(Q(1,1), PT(n,n), C(1,1))
  AB = 0.0d0
  ! Col 1: diag
  AB(2,1) = 2.0d0
  ! Col 2: super diag then diag
  AB(1,2) = 1.0d0; AB(2,2) = 3.0d0
  ! Col 3
  AB(1,3) = 1.0d0; AB(2,3) = 4.0d0
  ! Col 4
  AB(1,4) = 1.0d0; AB(2,4) = 5.0d0
  ! Col 5 (no diag entry since m=4)
  AB(1,5) = 1.0d0
  ! Col 6
  AB(1,6) = 1.0d0
  call DGBBRD('P', m, n, ncc, kl, ku, AB, ldab, D, E, Q, 1, PT, n, C, 1, WORK, info)
  call begin_test('wide_4x6_P')
  call print_array('D', D, min(m,n))
  call print_array('E', E, min(m,n)-1)
  call print_matrix('PT', PT, n, n, n)
  call print_int('info', info)
  call end_test()
  deallocate(AB, D, E, WORK, Q, PT, C)

  ! ============================================================
  ! Test 5: 4x4 lower bidiagonal (kl=1, ku=0), vect='B'
  m = 4; n = 4; kl = 1; ku = 0; ldab = 2; ncc = 0
  allocate(AB(ldab, n))
  allocate(D(min(m,n)), E(min(m,n)-1), WORK(2*max(m,n)))
  allocate(Q(m,m), PT(n,n), C(1,1))
  AB = 0.0d0
  ! Col 1
  AB(1,1) = 2.0d0; AB(2,1) = -1.0d0
  ! Col 2
  AB(1,2) = 3.0d0; AB(2,2) = -1.0d0
  ! Col 3
  AB(1,3) = 4.0d0; AB(2,3) = -1.0d0
  ! Col 4
  AB(1,4) = 5.0d0
  call DGBBRD('B', m, n, ncc, kl, ku, AB, ldab, D, E, Q, m, PT, n, C, 1, WORK, info)
  call begin_test('lower_4x4_B')
  call print_array('D', D, min(m,n))
  call print_array('E', E, min(m,n)-1)
  call print_matrix('Q', Q, m, m, m)
  call print_matrix('PT', PT, n, n, n)
  call print_int('info', info)
  call end_test()
  deallocate(AB, D, E, WORK, Q, PT, C)

  ! ============================================================
  ! Test 6: diagonal only (kl=0, ku=0), vect='N'
  m = 4; n = 4; kl = 0; ku = 0; ldab = 1; ncc = 0
  allocate(AB(ldab, n))
  allocate(D(min(m,n)), E(max(1,min(m,n)-1)), WORK(2*max(m,n)))
  allocate(Q(1,1), PT(1,1), C(1,1))
  AB(1,1) = 2.5d0
  AB(1,2) = -1.5d0
  AB(1,3) = 3.5d0
  AB(1,4) = 4.5d0
  call DGBBRD('N', m, n, ncc, kl, ku, AB, ldab, D, E, Q, 1, PT, 1, C, 1, WORK, info)
  call begin_test('diag_4x4_N')
  call print_array('D', D, min(m,n))
  call print_array('E', E, max(1,min(m,n)-1))
  call print_int('info', info)
  call end_test()
  deallocate(AB, D, E, WORK, Q, PT, C)

  ! ============================================================
  ! Test 7: m=0 quick return
  m = 0; n = 4; kl = 1; ku = 1; ldab = 3; ncc = 0
  allocate(AB(ldab,max(1,n)))
  allocate(D(1), E(1), WORK(max(1,2*max(m,n))))
  allocate(Q(1,1), PT(1,1), C(1,1))
  AB = 0.0d0
  call DGBBRD('N', m, n, ncc, kl, ku, AB, ldab, D, E, Q, 1, PT, 1, C, 1, WORK, info)
  call begin_test('m_zero')
  call print_int('info', info)
  call end_test()
  deallocate(AB, D, E, WORK, Q, PT, C)

end program
