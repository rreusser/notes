# Implementation Queue

Prioritized remaining d\*/z\* LAPACK routines, ordered by tier then by
completion progress. Items near the top of each tier are closest to done.

**Implemented: 485 modules (134 algorithms complete) | Remaining: 611 routines (265 algorithms)**

> To update this file after implementing a routine, run `/blahpack-status`
> or check the box manually. Regenerate with `node bin/gen-queue.js > QUEUE.md`.

## Core Solvers & Factorizations (107 routines)

- [ ] **ztgsyl** (3/4 done) — Solves the generalized Sylvester equation: A * R - L * B = scale * C (1) D * R - L * E = scale * F where R and L are unk
- [ ] **dgetf2, dpstf2, zgetf2, zpstf2** (9/13 done) — Computes the LU factorization of a general band matrix using the unblocked version of the algorithm.
- [ ] **dpteqr, zpteqr** (4/6 done) — Computes the eigenvalues of a Hessenberg matrix H and, optionally, the matrices T and Z from the Schur decomposition H =
- [ ] **ztgsy2** (2/3 done) — Solves the Sylvester matrix equation where the matrices are of order 1 or 2.
- [ ] **zgesc2** (1/2 done) — Solves a system of linear equations using the LU factorization with complete pivoting computed by sgetc2.
- [ ] **zgetc2** (1/2 done) — Computes the LU factorization with complete pivoting of the general n-by-n matrix.
- [ ] **zla_gbrpvgrw** (1/2 done) — Computes the reciprocal pivot growth factor norm(A)/norm(U) for a general banded matrix.
- [ ] **zla_gerpvgrw** (1/2 done) — Computes the reciprocal pivot growth factor norm(A)/norm(U).
- [ ] **zla_porpvgrw** (1/2 done) — Computes the reciprocal pivot growth factor norm(A)/norm(U) for a symmetric or Hermitian positive-definite matrix.
- [ ] **zla_syrpvgrw** (1/2 done) — Computes the reciprocal pivot growth factor norm(A)/norm(U) for a symmetric indefinite matrix.
- [ ] **zlaev2** (1/2 done) — Computes the eigenvalues and eigenvectors of a 2-by-2 symmetric/Hermitian matrix.
- [ ] **zlaqsy** (1/2 done) — Scales a symmetric/Hermitian matrix, using scaling factors computed by spoequ.
- [ ] **zlatdf** (1/2 done) — Uses the LU factorization of the n-by-n matrix computed by sgetc2 and computes a contribution to the reciprocal Dif-esti
- [ ] **ztgsja** (1/2 done) — Computes the generalized singular value decomposition (GSVD) of two real upper triangular (or trapezoidal) matrices A an
- [ ] **dbdsvdx, dgesvdx, zgesvdx** — Computes the singular value decomposition (SVD) of a real N-by-N (upper or lower) bidiagonal matrix B, B = U * S * VT, w
- [ ] **dgedmd, zgedmd** — Computes the Dynamic Mode Decomposition (DMD) for a pair of data snapshot matrices.
- [ ] **dgedmdq, zgedmdq** — Computes the Dynamic Mode Decomposition (DMD) for a pair of data snapshot matrices.
- [ ] **dgejsv, zgejsv** — Computes the singular value decomposition (SVD) of a real M-by-N matrix [A], where M >= N.
- [ ] **dgelq, zgelq** — Computes an LQ factorization of a real M-by-N matrix A: A = ( L 0 ) * Q where: Q is a N-by-N orthogonal matrix; L is a l
- [ ] **dgelqt3, zgelqt3** — Recursively computes a LQ factorization of a general real or complex matrix using the compact WY representation of Q.
- [ ] **dgelst, zgelst** — Solves overdetermined or underdetermined systems for GE matrices using QR or LQ factorization with compact WY representa
- [ ] **dgelsy, zgelsy** — Solves overdetermined or underdetermined systems for GE matrices
- [ ] **dgeql2, zgeql2** — Computes the QL factorization of a general rectangular matrix using an unblocked algorithm.
- [ ] **dgeqlf, zgeqlf** — Computes a QL factorization of a real M-by-N matrix A: A = Q * L.
- [ ] **dgeqp3rk, dlaqp3rk, zgeqp3rk, zlaqp3rk** — Computes a truncated Householder QR factorization with column pivoting of a real m-by-n matrix A by using Level 3 BLAS a
- [ ] **dgeqr, zgeqr** — Computes a QR factorization of a real M-by-N matrix A: A = Q * ( R ), ( 0 ) where: Q is a M-by-M orthogonal matrix; R is
- [ ] **dgeqr2p, zgeqr2p** — Computes the QR factorization of a general rectangular matrix with non-negative diagonal elements using an unblocked alg
- [ ] **dgeqrfp, zgeqrfp** — DGEQR2P computes a QR factorization of a real M-by-N matrix A: A = Q * ( R ), ( 0 ) where: Q is a M-by-M orthogonal matr
- [ ] **dgeqrt3, zgeqrt3** — Recursively computes a QR factorization of a general real or complex matrix using the compact WY representation of Q.
- [ ] **dgesdd, zgesdd** — Computes the singular value decomposition (SVD) of a real M-by-N matrix A, optionally computing the left and right singu
- [ ] **dgesvdq, zgesvdq** — Computes the singular value decomposition (SVD) with a QR-Preconditioned QR SVD Method for GE matrices
- [ ] **dgesvj, zgesvj** — Computes the singular value decomposition (SVD) of a real M-by-N matrix A, where M >= N.
- [ ] **dgetsls, zgetsls** — Solves overdetermined or underdetermined real linear systems involving an M-by-N matrix A, using a tall skinny QR or sho
- [ ] **dlagv2** — Computes the Generalized Schur factorization of a real 2-by-2 matrix pencil (A,B) where B is upper triangular.
- [ ] **dlals0, zlals0** — Applies back multiplying factors in solving the least squares problem using divide and conquer SVD approach. Used by sge
- [ ] **dlaqgb, zlaqgb** — Scales a general band matrix, using row and column scaling factors computed by sgbequ.
- [ ] **dlaqp2rk, zlaqp2rk** — Computes truncated QR factorization with column pivoting of a real matrix block using Level 2 BLAS and overwrites a real
- [ ] **dlaqsb, zlaqsb** — Scales a symmetric/Hermitian band matrix, using scaling factors computed by spbequ.
- [ ] **dlaqsp, zlaqsp** — Scales a symmetric/Hermitian matrix in packed storage, using scaling factors computed by sppequ.
- [ ] **dlaqtr** — Solves a real quasi-triangular system of equations, or a complex quasi-triangular system of special form, in real arithm
- [ ] **dlaqz0, zlaqz0** — Computes the eigenvalues of a real matrix pair (H,T), where H is an upper Hessenberg matrix and T is upper triangular, u
- [ ] **dlarmm** — Returns a factor s in (0, 1] such that the linear updates (s * C) - A * (s * B) and (s * C) - (s * A) * B cannot overflo
- [ ] **dlarrb** — Provides limited bisection to locate eigenvalues for more accuracy.
- [ ] **dlarrd** — Computes the eigenvalues of a symmetric tridiagonal matrix to suitable accuracy.
- [ ] **dlarrf** — Finds a new relatively robust representation such that at least one of the eigenvalues is relatively isolated.
- [ ] **dlarrk** — Computes one eigenvalue of a symmetric tridiagonal matrix T to suitable accuracy.
- [ ] **dlarrv, zlarrv** — Computes the eigenvectors of the tridiagonal matrix T = L D LT given L, D and the eigenvalues of L D LT.
- [ ] **dlarzt, zlarzt** — Forms the triangular factor T of a block reflector H = I - vtvH.
- [ ] **dlaswlq, zlaswlq** — Computes a blocked Tall-Skinny LQ factorization of a real M-by-N matrix A for M <= N: A = ( L 0 ) * Q, where: Q is a n-b
- [ ] **dlatps, zlatps** — Solves a triangular system of equations with the matrix held in packed storage.
- [ ] **dlatrs3, zlatrs3** — Solves a triangular system of equations with the scale factors set to prevent overflow.
- [ ] **dlatrz, zlatrz** — Factors an upper trapezoidal matrix by means of orthogonal transformations.
- [ ] **dorgr2, zungr2** — Generates all or part of the orthogonal matrix Q from an RQ factorization determined by sgerqf (unblocked algorithm).
- [ ] **dormr3, zunmr3** — Multiplies a general matrix by the orthogonal matrix from a RZ factorization determined by stzrzf (unblocked algorithm).
- [ ] **dsyconvf, zsyconvf** — If parameter WAY = 'C': DSYCONVF converts the factorization output format used in DSYTRF provided on entry in parameter 
- [ ] **dsytri2, zhetri2, zsytri2** — Computes the inverse of a DOUBLE PRECISION symmetric indefinite matrix A using the factorization A = U*D*U**T or A = L*D
- [ ] **dsytri2x, zhetri2x, zsytri2x** — Computes the inverse of a real symmetric indefinite matrix A using the factorization A = U*D*U**T or A = L*D*L**T comput
- [ ] **dtfsm, ztfsm** — Solves a matrix equation (one operand is a triangular matrix in RFP format).
- [ ] **zla_herpvgrw** — Computes the reciprocal pivot growth factor norm(A)/norm(U).
- [ ] **zlaesy** — Computes the eigenvalues and eigenvectors of a 2-by-2 complex symmetric matrix.
- [ ] **zlaqhb** — Scales a Hermitian band matrix, using scaling factors computed by cpbequ.

## Divide-and-Conquer (eigen/SVD) (30 routines)

- [ ] **dbdsdc** — Computes the singular value decomposition (SVD) of a real N-by-N (upper or lower) bidiagonal matrix B: B = U * S * VT, u
- [ ] **dlaed0, zlaed0** — Used by DSTEDC. Computes all eigenvalues and corresponding eigenvectors of an unreduced symmetric tridiagonal matrix usi
- [ ] **dlaed1** — Used by DSTEDC. Computes the updated eigensystem of a diagonal matrix after modification by a rank-one symmetric matrix.
- [ ] **dlaed2** — Used by DSTEDC. Merges eigenvalues and deflates secular equation. Used when the original matrix is tridiagonal.
- [ ] **dlaed3** — Used by DSTEDC. Finds the roots of the secular equation and updates the eigenvectors. Used when the original matrix is t
- [ ] **dlaed4** — Used by DSTEDC. Finds a single root of the secular equation.
- [ ] **dlaed6** — Used by DSTEDC. Computes one Newton step in solution of the secular equation.
- [ ] **dlaed7, zlaed7** — Used by DSTEDC. Computes the updated eigensystem of a diagonal matrix after modification by a rank-one symmetric matrix.
- [ ] **dlaed8, zlaed8** — Used by DSTEDC. Merges eigenvalues and deflates secular equation. Used when the original matrix is dense.
- [ ] **dlaed9** — Used by DSTEDC. Finds the roots of the secular equation and updates the eigenvectors. Used when the original matrix is d
- [ ] **dlaeda** — Used by DSTEDC. Computes the Z vector determining the rank-one modification of the diagonal matrix. Used when the origin
- [ ] **dlasd0** — Computes the singular values of a real upper bidiagonal n-by-m matrix B with diagonal d and off-diagonal e. Used by sbds
- [ ] **dlasd1** — Computes the SVD of an upper bidiagonal matrix B of the specified size. Used by sbdsdc.
- [ ] **dlasd2** — Merges the two sets of singular values together into a single sorted set. Used by sbdsdc.
- [ ] **dlasd3** — Finds all square roots of the roots of the secular equation, as defined by the values in D and Z, and then updates the s
- [ ] **dlasd4** — Computes the square root of the i-th updated eigenvalue of a positive symmetric rank-one modification to a positive diag
- [ ] **dlasd6** — Computes the SVD of an updated upper bidiagonal matrix obtained by merging two smaller ones by appending a row. Used by 
- [ ] **dlasd7** — Merges the two sets of singular values together into a single sorted set. Then it tries to deflate the size of the probl
- [ ] **dlasd8** — Finds the square roots of the roots of the secular equation, and stores, for each element in D, the distance to its two 
- [ ] **dlasda** — Computes the singular value decomposition (SVD) of a real upper bidiagonal matrix with diagonal d and off-diagonal e. Us
- [ ] **dlasdq** — Computes the SVD of a real bidiagonal matrix with diagonal d and off-diagonal e. Used by sbdsdc.
- [ ] **dstedc, zstedc** — Computes all eigenvalues and, optionally, eigenvectors of a symmetric tridiagonal matrix using the divide and conquer me
- [ ] **dstegr, zstegr** — Computes selected eigenvalues and, optionally, eigenvectors of a real symmetric tridiagonal matrix T.
- [ ] **dstemr, zstemr** — Computes selected eigenvalues and, optionally, eigenvectors of a real symmetric tridiagonal matrix T.

## Generalized Eigenvalue Problems (49 routines)

- [ ] **dggbak** (3/4 done) — Forms the right or left eigenvectors of a real general matrix by backward transformation on the computed eigenvectors of
- [ ] **dggbal** (3/4 done) — Balances a general real matrix A.
- [ ] **dgghrd** (3/4 done) — Reduces a real general matrix A to upper Hessenberg form H by an orthogonal similarity transformation: Q**T * A * Q = H 
- [ ] **zggqrf** (3/4 done) — Computes a QR factorization of a real M-by-N matrix A: A = Q * ( R ), ( 0 ) where: Q is a M-by-M orthogonal matrix; R is
- [ ] **dtgexc, ztgexc** (3/5 done) — Swaps adjacent diagonal blocks of a real upper quasi-triangular matrix in Schur canonical form, by an orthogonal similar
- [ ] **dggev, dsbev, dspev, zhbev, zhpev** (6/11 done) — Computes the eigenvalues and, optionally, the left and/or right eigenvectors for GE matrices
- [ ] **dgges, zgges** (2/4 done) — Computes the eigenvalues, the Schur form, and, optionally, the matrix of Schur vectors for GE matrices
- [ ] **dggrqf, zggrqf** (2/4 done) — Computes an RQ factorization of a real M-by-N matrix A: A = R * Q.
- [ ] **dhgeqz** (1/2 done) — Computes the eigenvalues of a real matrix pair (H,T), where H is an upper Hessenberg matrix and T is upper triangular, u
- [ ] **dtgsen, ztgsen** (2/4 done) — Reorders the generalized real Schur decomposition of a real matrix pair (A, B) (in terms of an orthonormal equivalence t
- [ ] **zggsvd3** (1/2 done) — Computes the singular value decomposition (SVD) for OTHER matrices
- [ ] **zggsvp3** (1/2 done) — Computes orthogonal matrices U, V and Q such that N-K-L K L U**T*A*Q = K ( 0 A12 A13 ) if M-K-L >= 0; L ( 0 0 A23 ) M-K-
- [ ] **dgeevx, dggevx, dsbevx, dspevx, zgeevx, zggevx, zhbevx, zhpevx** (3/11 done) — Computes the eigenvalues and, optionally, the left and/or right eigenvectors for GE matrices
- [ ] **ddisna, dtgsna, dtrsna, ztgsna, ztrsna** — Computes the reciprocal condition numbers for the eigenvectors of a real symmetric or complex Hermitian matrix or for th
- [ ] **dgeesx, dggesx, zgeesx, zggesx** — Computes the eigenvalues, the Schur form, and, optionally, the matrix of Schur vectors for GE matrices
- [ ] **dgges3, zgges3** — Computes the eigenvalues, the Schur form, and, optionally, the matrix of Schur vectors for GE matrices (blocked algorith
- [ ] **dggev3, zggev3** — Computes the eigenvalues and, optionally, the left and/or right eigenvectors for GE matrices (blocked algorithm)
- [ ] **dggglm, zggglm** — Solves a general Gauss-Markov linear model (GLM) problem: minimize || y ||_2 subject to d = A*x + B*y x where A is an N-
- [ ] **dgghd3, zgghd3** — Reduces a pair of real matrices (A,B) to generalized upper Hessenberg form using orthogonal transformations, where A is 
- [ ] **dgglse, zgglse** — Solves overdetermined or underdetermined systems for OTHER matrices
- [ ] **dtgex2, ztgex2** — Swaps adjacent diagonal blocks in an upper (quasi) triangular matrix pair by an orthogonal equivalence transformation.

## Banded & Packed Storage (111 routines)

- [ ] **dppsv, dspsv, zhpsv, zppsv, zspsv** (15/20 done) — Computes the solution to system of linear equations A * X = B for GB matrices (simple driver)
- [ ] **dpftrs, dpptrs, dsptrs, dtbtrs, dtptrs, zhptrs, zpftrs, zpptrs, zsptrs, ztbtrs, ztptrs** (19/30 done) — Solves a system of linear equations A * X = B or A**T * X = B with a general band matrix A using the LU factorization co
- [ ] **dpftrf, dpptrf, dpstrf, dsptrf, zhptrf, zpftrf, zpptrf, zpstrf, zsptrf** (15/24 done) — Computes an LU factorization of a real m-by-n band matrix A using partial pivoting with row interchanges.
- [ ] **dlacon, dppcon, dspcon, dtbcon, dtpcon, zhpcon, zlacon, zppcon, zspcon, ztbcon, ztpcon** (17/28 done) — Estimates the reciprocal of the condition number of a real general band matrix A, in either the 1-norm or the infinity-n
- [ ] **dsbtrd, dsptrd, zhbtrd, zhptrd** (4/8 done) — Reduces the first nb rows and columns of a symmetric/Hermitian matrix A to real tridiagonal form by an orthogonal simila
- [ ] **dgbrfs, dpbrfs, dpprfs, dsprfs, dtbrfs, dtprfs, zgbrfs, zgtrfs, zhprfs, zpbrfs, zpprfs, zsprfs, ztbrfs, ztprfs, ztrrfs** (11/26 done) — Improves the computed solution to a system of linear equations when the coefficient matrix is banded, and provides error
- [ ] **dgbsvx, dpbsvx, dppsvx, dptsvx, dspsvx, zgbsvx, zgtsvx, zhpsvx, zpbsvx, zppsvx, zptsvx, zspsvx** (8/20 done) — Computes the solution to system of linear equations A * X = B for GB matrices
- [ ] **dsbgst, dspgst, zhbgst, zhpgst** (2/6 done) — Reduces a real symmetric-definite banded generalized eigenproblem A*x = lambda*B*x to standard form C*y = lambda*y, such
- [ ] **dsbgv, dspgv, zhbgv, zhpgv** (2/6 done) — Computes all the eigenvalues, and optionally, the eigenvectors of a real generalized symmetric-definite banded eigenprob
- [ ] **dsbgvx, dspgvx, zhbgvx, zhpgvx** (2/6 done) — Computes selected eigenvalues, and optionally, eigenvectors of a real generalized symmetric-definite banded eigenproblem
- [ ] **dpftri, dpptri, dsptri, dsytri, dtftri, dtptri, zhetri, zhptri, zpftri, zpptri, zsptri, zsytri, ztftri, ztptri** (6/20 done) — Computes the inverse of a matrix using the LU factorization computed by DGETRF.
- [ ] **dpbstf, zpbstf** — Computes a split Cholesky factorization of a real symmetric positive definite band matrix A.
- [ ] **dsb2st_kernels, zhb2st_kernels** — Is an internal routine used by the DSYTRD_SB2ST subroutine.
- [ ] **dsbevd, dspevd, dstevd, dsyevd, zhbevd, zheevd, zhpevd** — Computes the eigenvalues and, optionally, the left and/or right eigenvectors for OTHER matrices
- [ ] **dsbgvd, dspgvd, dsygvd, zhbgvd, zhegvd, zhpgvd** — Computes all the eigenvalues, and optionally, the eigenvectors of a real generalized symmetric-definite banded eigenprob
- [ ] **dsposv** — Computes the solution to system of linear equations A * X = B for PO matrices

## Condition Numbers, Norms & Refinement (80 routines)

- [ ] **dgbequ, zgbequ** (8/10 done) — Computes row and column scalings intended to equilibrate an M-by-N band matrix A and reduce its condition number.
- [ ] **dgbbrd, zgbbrd** (4/6 done) — Reduces a real general m-by-n band matrix A to upper bidiagonal form B by an orthogonal transformation: Q**T * A * P = B
- [ ] **zrscl** (2/3 done) — Multiplies a vector by the reciprocal of a real scalar.
- [ ] **dgemlq, zgemlq** (2/4 done) — Overwrites the general real M-by-N matrix C with SIDE = 'L' SIDE = 'R' TRANS = 'N': Q * C C * Q TRANS = 'T': Q**T * C C 
- [ ] **dgemqr, zgemqr** (2/4 done) — Overwrites the general real M-by-N matrix C with SIDE = 'L' SIDE = 'R' TRANS = 'N': Q * C C * Q TRANS = 'T': Q**T * C C 
- [ ] **dopgtr, zupgtr** (2/4 done) — Generates a real orthogonal matrix Q which is defined as the product of n-1 elementary reflectors H(i) of order n, as re
- [ ] **dopmtr, zupmtr** (2/4 done) — Overwrites the general real M-by-N matrix C with SIDE = 'L' SIDE = 'R' TRANS = 'N': Q * C C * Q TRANS = 'T': Q**T * C C 
- [ ] **dtgevc, dtrevc, ztrevc** (1/4 done) — Computes some or all of the right and/or left eigenvectors of a pair of real matrices (S,P), where S is a quasi-triangul
- [ ] **dgbrfsx, dgerfsx, dporfsx, dsyrfsx, zgbrfsx, zgerfsx, zherfsx, zporfsx, zsyrfsx** — Improves the computed solution to a system of linear equations and provides error bounds and backward error estimates fo
- [ ] **dgsvj0, zgsvj0** — Pre-processor for the routine dgesvj.
- [ ] **dgsvj1, zgsvj1** — Pre-processor for the routine dgesvj, applies Jacobi rotations targeting only particular pivots.
- [ ] **dla_gbamv, zla_gbamv** — Performs a matrix-vector operation to calculate error bounds.
- [ ] **dla_gbrcond** — Estimates the Skeel condition number for a general banded matrix.
- [ ] **dla_gbrfsx_extended, zla_gbrfsx_extended** — Improves the computed solution to a system of linear equations for general banded matrices by performing extra-precise i
- [ ] **dla_geamv, zla_geamv** — Computes a matrix-vector product using a general matrix to calculate error bounds.
- [ ] **dla_gercond** — Estimates the Skeel condition number for a general matrix.
- [ ] **dla_gerfsx_extended, zla_gerfsx_extended** — Improves the computed solution to a system of linear equations for general matrices by performing extra-precise iterativ
- [ ] **dla_lin_berr, zla_lin_berr** — Computes a component-wise relative backward error.
- [ ] **dla_porcond** — Estimates the Skeel condition number for a symmetric positive-definite matrix.
- [ ] **dla_porfsx_extended, zla_porfsx_extended** — Improves the computed solution to a system of linear equations for symmetric or Hermitian positive-definite matrices by 
- [ ] **dla_syamv, zla_syamv** — Computes a matrix-vector product using a symmetric indefinite matrix to calculate error bounds.
- [ ] **dla_syrcond** — Estimates the Skeel condition number for a symmetric indefinite matrix.
- [ ] **dla_syrfsx_extended, zla_syrfsx_extended** — Improves the computed solution to a system of linear equations for symmetric indefinite matrices by performing extra-pre
- [ ] **dorgrq, zungrq** — Generates an M-by-N real matrix Q with orthonormal rows, which is defined as the last M rows of a product of K elementar
- [ ] **dorhr_col, zunhr_col** — Takes an M-by-N real matrix Q_in with orthonormal columns as input, stored in A, and performs Householder Reconstruction
- [ ] **dorm22, zunm22** — Multiplies a general matrix by a banded orthogonal matrix.
- [ ] **dormrz, zunmrz** — Overwrites the general real M-by-N matrix C with SIDE = 'L' SIDE = 'R' TRANS = 'N': Q * C C * Q TRANS = 'T': Q**T * C C 
- [ ] **dsfrk, zhfrk** — Performs a symmetric rank-k operation for matrix in RFP format.
- [ ] **dsyswapr, zheswapr, zsyswapr** — Applies an elementary permutation on the rows and columns of a symmetric matrix.
- [ ] **dsytrd_sy2sb** — Reduces a real symmetric matrix A to real symmetric band-diagonal form AB by a orthogonal similarity transformation: Q**
- [ ] **dtrsyl3, ztrsyl3** — 
- [ ] **dtzrzf, ztzrzf** — Reduces the M-by-N ( M<=N ) real upper trapezoidal matrix A to upper triangular form by means of orthogonal transformati
- [ ] **zhetrd_he2hb** — Reduces a complex Hermitian matrix A to complex Hermitian band-diagonal form AB by a unitary similarity transformation: 
- [ ] **zla_gbrcond_c** — Computes the infinity norm condition number of op(A)*inv(diag(c)) for general banded matrices.
- [ ] **zla_gbrcond_x** — Computes the infinity norm condition number of op(A)*diag(x) for general banded matrices.
- [ ] **zla_gercond_c** — Computes the infinity norm condition number of op(A)*inv(diag(c)) for general matrices.
- [ ] **zla_gercond_x** — Computes the infinity norm condition number of op(A)*diag(x) for general matrices.
- [ ] **zla_heamv** — Computes a matrix-vector product using a Hermitian indefinite matrix to calculate error bounds.
- [ ] **zla_hercond_c** — Computes the infinity norm condition number of op(A)*inv(diag(c)) for Hermitian indefinite matrices.
- [ ] **zla_hercond_x** — Computes the infinity norm condition number of op(A)*diag(x) for Hermitian indefinite matrices.
- [ ] **zla_herfsx_extended** — Improves the computed solution to a system of linear equations for Hermitian indefinite matrices by performing extra-pre
- [ ] **zla_porcond_c** — Computes the infinity norm condition number of op(A)*inv(diag(c)) for Hermitian positive-definite matrices.
- [ ] **zla_porcond_x** — Computes the infinity norm condition number of op(A)*diag(x) for Hermitian positive-definite matrices.
- [ ] **zla_syrcond_c** — Computes the infinity norm condition number of op(A)*inv(diag(c)) for symmetric indefinite matrices.
- [ ] **zla_syrcond_x** — Computes the infinity norm condition number of op(A)*diag(x) for symmetric indefinite matrices.

## Auxiliary Routines (86 routines)

- [ ] **zlags2** (3/4 done) — Computes 2-by-2 orthogonal matrices U, V, and Q, and applies them to matrices A and B such that the rows of the transfor
- [ ] **zlapll** (1/2 done) — Measures the linear dependence of two vectors.
- [ ] **zlarfx** (1/2 done) — Applies an elementary reflector to a general rectangular matrix, with loop unrolling when the reflector has order ≤ 10.
- [ ] **zlargv** (1/2 done) — Generates a vector of plane rotations with real cosines and real sines.
- [ ] **zlarnv** (1/2 done) — Returns a vector of random numbers from a uniform or normal distribution.
- [ ] **zlarscl2** (1/2 done) — Performs reciprocal diagonal scaling on a matrix.
- [ ] **dlangb, dlanhs, dlansb, dlansf, dlansp, dlantb, dlantp, zlangb, zlangt, zlanhb, zlanhf, zlanhp, zlansb, zlansp, zlantb, zlantp** (10/26 done) — Returns the value of the 1-norm, Frobenius norm, infinity-norm, or the largest absolute value of any element of general 
- [ ] **dhsein, dlaein, zhsein, zlaein** (2/6 done) — Uses inverse iteration to find specified right and/or left eigenvectors of a real upper Hessenberg matrix H.
- [ ] **dgelsd, dlalsd, zgelsd, zlalsd** — Computes the minimum-norm solution to a linear least squares problem for GE matrices
- [ ] **dlag2s** — Converts a double precision matrix to a single precision matrix.
- [ ] **dlaic1, zlaic1** — Applies one step of incremental condition estimation.
- [ ] **dlalsa, zlalsa** — Computes the SVD of the coefficient matrix in compact form. Used by sgelsd.
- [ ] **dlamswlq, zlamswlq** — Overwrites the general real M-by-N matrix C with SIDE = 'L' SIDE = 'R' TRANS = 'N': Q * C C * Q TRANS = 'T': Q**T * C C 
- [ ] **dlamtsqr, zlamtsqr** — Overwrites the general real M-by-N matrix C with SIDE = 'L' SIDE = 'R' TRANS = 'N': Q * C C * Q TRANS = 'T': Q**T * C C 
- [ ] **dlaneg** — Computes the Sturm count.
- [ ] **dlaorhr_col_getrfnp** — Computes the modified LU factorization without pivoting of a real general M-by-N matrix A.
- [ ] **dlaorhr_col_getrfnp2** — Computes the modified LU factorization without pivoting of a real general M-by-N matrix A.
- [ ] **dlaqz1, zlaqz1** — Given a 3-by-3 matrix pencil (A,B), DLAQZ1 sets v to a scalar multiple of the first column of the product (*) K = (A - (
- [ ] **dlaqz2, zlaqz2** — Chases a 2x2 shift bulge in a matrix pencil down a single position
- [ ] **dlaqz3, zlaqz3** — Performs AED
- [ ] **dlaqz4** — Executes a single multishift QZ sweep
- [ ] **dlar1v, zlar1v** — Computes the (scaled) r-th column of the inverse of the submatrix in rows b1 through bn of the tridiagonal matrix LDLT -
- [ ] **dlarfb_gett, zlarfb_gett** — Applies a real Householder block reflector H from the left to a real (K+M)-by-N "triangular-pentagonal" matrix composed 
- [ ] **dlarfgp, zlarfgp** — Generates an elementary reflector (Householder matrix) with non-negative beta.
- [ ] **dlarfy, zlarfy** — Applies an elementary reflector, or Householder matrix, H, to an n x n symmetric matrix C, from both the left and the ri
- [ ] **dlarre** — Given the tridiagonal matrix T, sets small off-diagonal elements to zero and for each unreduced block Ti, finds base rep
- [ ] **dlarrr** — Performs tests to decide whether the symmetric tridiagonal matrix T warrants expensive computations which guarantee high
- [ ] **dlartgp** — Generates a plane rotation so that the diagonal is nonnegative.
- [ ] **dlartgs** — Generates a plane rotation designed to introduce a bulge in implicit QR iteration for the bidiagonal SVD problem.
- [ ] **dlarz, zlarz** — Applies an elementary reflector (as returned by stzrzf) to a general matrix.
- [ ] **dlarzb, zlarzb** — Applies a block reflector or its transpose to a general matrix.
- [ ] **dlasyf_aa, zlasyf_aa** — DLATRF_AA factorizes a panel of a real symmetric matrix A using the Aasen's algorithm.
- [ ] **dlasyf_rk, zlasyf_rk** — Computes a partial factorization of a real symmetric indefinite matrix using bounded Bunch-Kaufman (rook) diagonal pivot
- [ ] **dlasyf_rook, zlasyf_rook** — *> DLASYF_ROOK computes a partial factorization of a real symmetric matrix using the bounded Bunch-Kaufman ("rook") diag
- [ ] **dlat2s** — Converts a double-precision triangular matrix to a single-precision triangular matrix.
- [ ] **dlatsqr, zlatsqr** — Computes a blocked Tall-Skinny QR factorization of a real M-by-N matrix A for M >= N: A = Q * ( R ), ( 0 ) where: Q is a
- [ ] **zlacrm** — Multiplies a complex matrix by a square real matrix.
- [ ] **zlacrt** — Performs a linear transformation of a pair of complex vectors.
- [ ] **zlag2c** — Converts a complex double precision matrix to a complex single precision matrix.
- [ ] **zlahef_aa** — DLAHEF_AA factorizes a panel of a complex hermitian matrix A using the Aasen's algorithm.
- [ ] **zlahef_rk** — Computes a partial factorization of a complex Hermitian indefinite matrix using bounded Bunch-Kaufman (rook) diagonal pi
- [ ] **zlahef_rook** — Computes a partial factorization of a complex Hermitian indefinite matrix using the bounded Bunch-Kaufman ("rook") diago
- [ ] **zlanht** — Returns the value of the 1-norm, or the Frobenius norm, or the infinity norm, or the element of largest absolute value o
- [ ] **zlaqhp** — Scales a Hermitian matrix stored in packed form.
- [ ] **zlarcm** — Copies all or part of a real two-dimensional array to a complex array.
- [ ] **zlat2c** — Converts a double complex triangular matrix to a complex triangular matrix.
- [ ] **zlaunhr_col_getrfnp** — Computes the modified LU factorization without pivoting of a complex general M-by-N matrix A.
- [ ] **zlaunhr_col_getrfnp2** — Computes the modified LU factorization without pivoting of a complex general M-by-N matrix A.

## Expert Drivers (9 routines)

- [ ] **dgbsvxx, dgesvxx, dposvxx, dsysvxx, zgbsvxx, zgesvxx, zhesvxx, zposvxx, zsysvxx** — Computes the solution to system of linear equations A * X = B for GB matrices

## Variants, TSQR, Mixed-Precision & CSD (139 routines)

- [ ] **dtprfb, ztprfb** (2/4 done) — Applies a block reflector or its transpose to a general rectangular matrix.
- [ ] **dbbcsd, dorcsd, zbbcsd, zuncsd** — Computes the CS decomposition of an orthogonal matrix in bidiagonal-block form, [ B11 | B12 0 0 ] [ 0 | 0 -I 0 ] X = [--
- [ ] **dgbequb, dgeequb, dpoequb, dsyequb, zgbequb, zgeequb, zheequb, zpoequb, zsyequb** — Computes row and column scalings intended to equilibrate an M-by-N matrix A and reduce its condition number.
- [ ] **dgelqt, dtplqt, zgelqt, ztplqt** — Computes a blocked LQ factorization of a real M-by-N matrix A using the compact WY representation of Q.
- [ ] **dgemlqt, dtpmlqt, zgemlqt, ztpmlqt** — Overwrites the general real M-by-N matrix C with SIDE = 'L' SIDE = 'R' TRANS = 'N': Q C C Q TRANS = 'T': Q**T C C Q**T w
- [ ] **dgemqrt, dtpmqrt, zgemqrt, ztpmqrt** — Overwrites the general real M-by-N matrix C with SIDE = 'L' SIDE = 'R' TRANS = 'N': Q C C Q TRANS = 'T': Q**T C C Q**T w
- [ ] **dgeqrt, dtpqrt, zgeqrt, ztpqrt** — Computes a blocked QR factorization of a real M-by-N matrix A using the compact WY representation of Q.
- [ ] **dgeqrt2, dtpqrt2, zgeqrt2, ztpqrt2** — Computes a QR factorization of a general real or complex matrix using the compact WY representation of Q.
- [ ] **dgetsqrhrt, zgetsqrhrt** — Computes a NB2-sized column blocked QR-factorization of a real M-by-N matrix A with M >= N, A = Q * R.
- [ ] **dorbdb, zunbdb** — Simultaneously bidiagonalizes the blocks of an M-by-M partitioned orthogonal matrix X: [ B11 | B12 0 0 ] [ X11 | X12 ] [
- [ ] **dorbdb1, zunbdb1** — Simultaneously bidiagonalizes the blocks of a tall and skinny matrix X with orthonormal columns: [ B11 ] [ X11 ] [ P1 | 
- [ ] **dorbdb2, zunbdb2** — Simultaneously bidiagonalizes the blocks of a tall and skinny matrix X with orthonormal columns: [ B11 ] [ X11 ] [ P1 | 
- [ ] **dorbdb3, zunbdb3** — Simultaneously bidiagonalizes the blocks of a tall and skinny matrix X with orthonormal columns: [ B11 ] [ X11 ] [ P1 | 
- [ ] **dorbdb4, zunbdb4** — Simultaneously bidiagonalizes the blocks of a tall and skinny matrix X with orthonormal columns: [ B11 ] [ X11 ] [ P1 | 
- [ ] **dorbdb5, zunbdb5** — Orthogonalizes the column vector X = [ X1 ] [ X2 ] with respect to the columns of Q = [ Q1 ] .
- [ ] **dorbdb6, zunbdb6** — Orthogonalizes the column vector X = [ X1 ] [ X2 ] with respect to the columns of Q = [ Q1 ] .
- [ ] **dorcsd2by1, zuncsd2by1** — Computes the CS decomposition of an M-by-Q matrix X with orthonormal columns that has been partitioned into a 2-by-1 blo
- [ ] **dorgtsqr, zungtsqr** — Generates an M-by-N real matrix Q_out with orthonormal columns, which are the first N columns of a product of real ortho
- [ ] **dorgtsqr_row, zungtsqr_row** — Generates an M-by-N real matrix Q_out with orthonormal columns from the output of DLATSQR.
- [ ] **dsbev_2stage, dsyev_2stage, zhbev_2stage, zheev_2stage** — Computes the eigenvalues and, optionally, the left and/or right eigenvectors for OTHER matrices
- [ ] **dsbevd_2stage, dsyevd_2stage, zhbevd_2stage, zheevd_2stage** — Computes the eigenvalues and, optionally, the left and/or right eigenvectors for OTHER matrices
- [ ] **dsbevx_2stage, dsyevx_2stage, zhbevx_2stage, zheevx_2stage** — Computes the eigenvalues and, optionally, the left and/or right eigenvectors for OTHER matrices
- [ ] **dsgesv** — Computes the solution to system of linear equations A * X = B for GE matrices (mixed precision with iterative refinement
- [ ] **dsycon_3, zhecon_3, zsycon_3** — Estimates the reciprocal of the condition number (in the 1-norm) of a real symmetric matrix A using the factorization co
- [ ] **dsycon_rook, zhecon_rook, zsycon_rook** — DSYCON_ROOK
- [ ] **dsyconvf_rook, zsyconvf_rook** — If parameter WAY = 'C': DSYCONVF_ROOK converts the factorization output format used in DSYTRF_ROOK provided on entry in 
- [ ] **dsyevr_2stage, zheevr_2stage** — Computes the eigenvalues and, optionally, the left and/or right eigenvectors for SY matrices
- [ ] **dsygv_2stage, zhegv_2stage** — Computes all the eigenvalues, and optionally, the eigenvectors of a real generalized symmetric-definite eigenproblem, of
- [ ] **dsysv_aa, zhesv_aa, zsysv_aa** — Computes the solution to system of linear equations A * X = B for SY matrices
- [ ] **dsysv_aa_2stage, zhesv_aa_2stage, zsysv_aa_2stage** — Computes the solution to system of linear equations A * X = B for SY matrices
- [ ] **dsysv_rk, zhesv_rk, zsysv_rk** — Computes the solution to system of linear equations A * X = B for SY matrices
- [ ] **dsysv_rook, zhesv_rook, zsysv_rook** — Computes the solution to system of linear equations A * X = B for SY matrices
- [ ] **dsytf2_rk, zhetf2_rk, zsytf2_rk** — Computes the factorization of a real symmetric indefinite matrix using the bounded Bunch-Kaufman (rook) diagonal pivotin
- [ ] **dsytf2_rook, zhetf2_rook, zsytf2_rook** — Computes the factorization of a real symmetric indefinite matrix using the bounded Bunch-Kaufman ("rook") diagonal pivot
- [ ] **dsytrd_2stage, zhetrd_2stage** — Reduces a real symmetric matrix A to real symmetric tridiagonal form T by a orthogonal similarity transformation: Q1**T 
- [ ] **dsytrf_aa, zhetrf_aa, zsytrf_aa** — Computes the factorization of a real symmetric matrix A using the Aasen's algorithm.
- [ ] **dsytrf_aa_2stage, zhetrf_aa_2stage, zsytrf_aa_2stage** — Computes the factorization of a real symmetric matrix A using the Aasen's algorithm.
- [ ] **dsytrf_rk, zhetrf_rk, zsytrf_rk** — Computes the factorization of a real symmetric indefinite matrix using the bounded Bunch-Kaufman (rook) diagonal pivotin
- [ ] **dsytrf_rook, zhetrf_rook, zsytrf_rook** — Computes the factorization of a real symmetric matrix A using the bounded Bunch-Kaufman ("rook") diagonal pivoting metho
- [ ] **dsytri_3, zhetri_3, zsytri_3** — Computes the inverse of a real symmetric indefinite matrix A using the factorization computed by DSYTRF_RK or DSYTRF_BK:
- [ ] **dsytri_3x, zhetri_3x, zsytri_3x** — Computes the inverse of a real symmetric indefinite matrix A using the factorization computed by DSYTRF_RK or DSYTRF_BK:
- [ ] **dsytri_rook, zhetri_rook, zsytri_rook** — Computes the inverse of a real symmetric matrix A using the factorization A = U*D*U**T or A = L*D*L**T computed by DSYTR
- [ ] **dsytrs_3, zhetrs_3, zsytrs_3** — Solves a system of linear equations A * X = B with a real symmetric matrix A using the factorization computed by DSYTRF_
- [ ] **dsytrs_aa, zhetrs_aa, zsytrs_aa** — Solves a system of linear equations A*X = B with a real symmetric matrix A using the factorization A = U**T*T*U or A = L
- [ ] **dsytrs_aa_2stage, zhetrs_aa_2stage, zsytrs_aa_2stage** — Solves a system of linear equations A*X = B with a real symmetric matrix A using the factorization A = U**T*T*U or A = L
- [ ] **dsytrs_rook, zhetrs_rook, zsytrs_rook** — Solves a system of linear equations A*X = B with a real symmetric matrix A using the factorization A = U*D*U**T or A = L
- [ ] **dtplqt2, ztplqt2** — Computes a LQ factorization of a real or complex "triangular-pentagonal" matrix, which is composed of a triangular block
- [ ] **zcgesv** — Computes the solution to system of linear equations A * X = B for GE matrices (mixed precision with iterative refinement
- [ ] **zcposv** — Computes the solution to system of linear equations A * X = B for PO matrices

---
**Total remaining: 611 routines across 265 algorithms**
