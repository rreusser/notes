'use strict';

// MODULES //

var Float64Array = require( '@stdlib/array/float64' );
var dgeqrf = require( '../../dgeqrf/lib/base.js' );
var dormqr = require( '../../dormqr/lib/base.js' );
var dgelqf = require( '../../dgelqf/lib/base.js' );
var dormlq = require( '../../dormlq/lib/base.js' );
var dgebrd = require( '../../dgebrd/lib/base.js' );
var dormbr = require( '../../dormbr/lib/base.js' );
var dorgbr = require( '../../dorgbr/lib/base.js' );
var dbdsqr = require( '../../dbdsqr/lib/base.js' );
var dlacpy = require( '../../dlacpy/lib/base.js' );
var dlascl = require( '../../dlascl/lib/base.js' );
var dlaset = require( '../../dlaset/lib/base.js' );
var dlange = require( '../../dlange/lib/base.js' );
var dlamch = require( '../../dlamch/lib/base.js' );
var dgemm = require( '../../../../blas/base/dgemm/lib/base.js' );
var dgemv = require( '../../../../blas/base/dgemv/lib/base.js' );
var dcopy = require( '../../../../blas/base/dcopy/lib/base.js' );
var drscl = require( '../../drscl/lib/base.js' );


// VARIABLES //

var NB = 32;            // Hardcoded block size (replaces ILAENV queries)
var MNTHR_RATIO = 1.6;  // ILAENV(6, 'DGELSS') threshold ratio M/N


// MAIN //

/**
* Computes the minimum norm solution to a real linear least squares problem:
*
*   minimize 2-norm(|| b - A*x ||)
*
* using the singular value decomposition (SVD) of A. A is an M-by-N matrix
* which may be rank-deficient.
*
* Several right hand side vectors b and solution vectors x can be handled
* in a single call; they are stored as the columns of the M-by-NRHS right
* hand side matrix B and the N-by-NRHS solution matrix X.
*
* The effective rank of A is determined by treating as zero those singular
* values which are less than RCOND times the largest singular value.
*
* @private
* @param {NonNegativeInteger} M - number of rows of A
* @param {NonNegativeInteger} N - number of columns of A
* @param {NonNegativeInteger} nrhs - number of right hand sides (columns of B)
* @param {Float64Array} A - M-by-N matrix, overwritten on exit
* @param {integer} strideA1 - stride of the first dimension of A
* @param {integer} strideA2 - stride of the second dimension of A
* @param {NonNegativeInteger} offsetA - starting index for A
* @param {Float64Array} B - on entry, M-by-NRHS (or max(M,N)-by-NRHS) RHS matrix;
*   on exit, N-by-NRHS solution matrix (rows max(M,N)+1..N zeroed if M < N)
* @param {integer} strideB1 - stride of the first dimension of B
* @param {integer} strideB2 - stride of the second dimension of B
* @param {NonNegativeInteger} offsetB - starting index for B
* @param {Float64Array} S - output array of singular values in decreasing order (length min(M,N))
* @param {integer} strideS - stride length for S
* @param {NonNegativeInteger} offsetS - starting index for S
* @param {number} rcond - used to determine the effective rank of A.
*   Singular values S(i) <= RCOND*S(1) are treated as zero.
*   If RCOND < 0, machine precision is used instead.
* @param {Array} rank - output array; rank[0] set to the effective rank of A
* @param {Float64Array} WORK - workspace array (if null, allocated internally)
* @param {integer} strideWORK - stride length for WORK
* @param {NonNegativeInteger} offsetWORK - starting index for WORK
* @param {integer} lwork - length of WORK array (if 0 or WORK is null, auto-sized)
* @returns {integer} info - 0 if successful, >0 if DBDSQR did not converge
*/
function dgelss( M, N, nrhs, A, strideA1, strideA2, offsetA, B, strideB1, strideB2, offsetB, S, strideS, offsetS, rcond, rank, WORK, strideWORK, offsetWORK, lwork ) { // eslint-disable-line max-len, max-params
	var ldwork;
	var bignum;
	var smlnum;
	var minmn;
	var maxmn;
	var mnthr;
	var iascl;
	var ibscl;
	var itauq;
	var itaup;
	var iwork;
	var chunk;
	var sfmin;
	var anrm;
	var bnrm;
	var info;
	var itau;
	var DUM;
	var thr;
	var eps;
	var mm;
	var ie;
	var il;
	var bl;
	var i;

	minmn = Math.min( M, N );
	maxmn = Math.max( M, N );
	mnthr = Math.round( MNTHR_RATIO * Math.min( M, N ) );

	// Quick return if dimensions are zero
	if ( M === 0 || N === 0 ) {
		rank[ 0 ] = 0;
		return 0;
	}

	// Allocate workspace if not provided
	if ( !WORK || lwork === 0 ) {
		lwork = computeWorkSize( M, N, nrhs, mnthr );
		WORK = new Float64Array( lwork );
		strideWORK = 1;
		offsetWORK = 0;
	}

	// Get machine parameters
	eps = dlamch( 'P' );
	sfmin = dlamch( 'S' );
	smlnum = sfmin / eps;
	bignum = 1.0 / smlnum;

	// DUM is a dummy array for dbdsqr (unused VT/U matrix when ncvt=0 or nru=0)
	DUM = new Float64Array( 1 );

	// Scale A if max element is outside [smlnum, bignum]
	anrm = dlange( 'M', M, N, A, strideA1, strideA2, offsetA, WORK, strideWORK, offsetWORK );
	iascl = 0;
	if ( anrm > 0.0 && anrm < smlnum ) {
		// Scale matrix norm up to smlnum
		dlascl( 'G', 0, 0, anrm, smlnum, M, N, A, strideA1, strideA2, offsetA );
		iascl = 1;
	} else if ( anrm > bignum ) {
		// Scale matrix norm down to bignum
		dlascl( 'G', 0, 0, anrm, bignum, M, N, A, strideA1, strideA2, offsetA );
		iascl = 2;
	} else if ( anrm === 0.0 ) {
		// Matrix all zero. Return zero solution.
		dlaset( 'F', maxmn, nrhs, 0.0, 0.0, B, strideB1, strideB2, offsetB );
		dlaset( 'F', minmn, 1, 0.0, 0.0, S, strideS, 1, offsetS );
		rank[ 0 ] = 0;
		return 0;
	}

	// Scale B
	bnrm = dlange( 'M', M, nrhs, B, strideB1, strideB2, offsetB, WORK, strideWORK, offsetWORK );
	ibscl = 0;
	if ( bnrm > 0.0 && bnrm < smlnum ) {
		// Scale matrix norm up to smlnum
		dlascl( 'G', 0, 0, bnrm, smlnum, M, nrhs, B, strideB1, strideB2, offsetB );
		ibscl = 1;
	} else if ( bnrm > bignum ) {
		// Scale matrix norm down to bignum
		dlascl( 'G', 0, 0, bnrm, bignum, M, nrhs, B, strideB1, strideB2, offsetB );
		ibscl = 2;
	}

	// ==========================================
	// Path 1: M >= N
	// ==========================================
	if ( M >= N ) {
		mm = M;
		if ( M >= mnthr ) {
			// Path 1a: M >> N — QR factorization first to reduce to N-by-N
			mm = N;
			itau = 0;       // TAU starts at WORK[0], length N
			iwork = itau + N;

			// Compute A = Q * R
			dgeqrf( M, N, A, strideA1, strideA2, offsetA,
				WORK, 1, itau,
				WORK, 1, iwork );

			// Multiply B := Q^T * B
			dormqr( 'L', 'T', M, nrhs, N,
				A, strideA1, strideA2, offsetA,
				WORK, 1, itau,
				B, strideB1, strideB2, offsetB,
				WORK, 1, iwork, lwork - iwork );

			// Zero out below-diagonal of R
			if ( N > 1 ) {
				dlaset( 'L', N - 1, N - 1, 0.0, 0.0,
					A, strideA1, strideA2, offsetA + strideA1 );
			}
		}

		ie = 0;          // E starts at WORK[0], length N
		itauq = ie + N;  // TAUQ starts at WORK[ie+N], length N
		itaup = itauq + N; // TAUP starts at WORK[itauq+N], length N
		iwork = itaup + N; // scratch space after TAUP

		// Bidiagonal reduction: A = Q_b * B_bd * P_b^T (of the mm-by-N matrix)
		dgebrd( mm, N, A, strideA1, strideA2, offsetA,
			S, strideS, offsetS,
			WORK, 1, ie,
			WORK, 1, itauq,
			WORK, 1, itaup,
			WORK, 1, iwork, lwork - iwork );

		// Multiply B by transpose of left bidiagonal transformation: B := Q_b^T * B
		dormbr( 'Q', 'L', 'T', mm, nrhs, N,
			A, strideA1, strideA2, offsetA,
			WORK, 1, itauq,
			B, strideB1, strideB2, offsetB,
			WORK, 1, iwork, lwork - iwork );

		// Generate right bidiagonal transformation: P_b^T stored in A
		dorgbr( 'P', N, N, N,
			A, strideA1, strideA2, offsetA,
			WORK, 1, itaup,
			WORK, 1, iwork, lwork - iwork );
		iwork = ie + N;

		// Compute SVD of bidiagonal: S = singular values, A = P_b^T (right singular vectors)
		// B is multiplied by Q_b^T on the left
		info = dbdsqr( 'U', N, N, 0, nrhs,
			S, strideS, offsetS,
			WORK, 1, ie,
			A, strideA1, strideA2, offsetA,
			DUM, 1, 1, 0,
			B, strideB1, strideB2, offsetB,
			WORK, 1, iwork );
		if ( info !== 0 ) {
			rank[ 0 ] = 0;
			return info;
		}

		// Threshold for rank determination
		thr = Math.max( rcond * S[ offsetS ], sfmin );
		if ( rcond < 0.0 ) {
			thr = Math.max( eps * S[ offsetS ], sfmin );
		}
		rank[ 0 ] = 0;
		for ( i = 0; i < N; i++ ) {
			if ( S[ offsetS + i * strideS ] > thr ) {
				// Scale the corresponding row of B by 1/S(i)
				drscl( nrhs, S[ offsetS + i * strideS ],
					B, strideB2, offsetB + i * strideB1 );
				rank[ 0 ] += 1;
			} else {
				// Zero out the corresponding row of B
				dlaset( 'F', 1, nrhs, 0.0, 0.0,
					B, strideB1, strideB2, offsetB + i * strideB1 );
			}
		}

		// Multiply by right singular vectors: X = V^T * (Sigma^+ * Q_b^T * B)
		// A now contains V^T (the right singular vectors), so X = A^T * B
		if ( lwork >= strideB2 * nrhs && nrhs > 1 ) {
			dgemm( 'T', 'N', N, nrhs, N, 1.0,
				A, strideA1, strideA2, offsetA,
				B, strideB1, strideB2, offsetB,
				0.0, WORK, 1, N, 0 );
			dlacpy( 'G', N, nrhs,
				WORK, 1, N, 0,
				B, strideB1, strideB2, offsetB );
		} else if ( nrhs > 1 ) {
			chunk = Math.max( 1, Math.floor( lwork / N ) );
			for ( i = 0; i < nrhs; i += chunk ) {
				bl = Math.min( nrhs - i, chunk );
				dgemm( 'T', 'N', N, bl, N, 1.0,
					A, strideA1, strideA2, offsetA,
					B, strideB1, strideB2, offsetB + i * strideB2,
					0.0, WORK, 1, N, 0 );
				dlacpy( 'G', N, bl,
					WORK, 1, N, 0,
					B, strideB1, strideB2, offsetB + i * strideB2 );
			}
		} else if ( nrhs === 1 ) {
			dgemv( 'T', N, N, 1.0,
				A, strideA1, strideA2, offsetA,
				B, strideB1, offsetB,
				0.0, WORK, 1, 0 );
			dcopy( N, WORK, 1, 0, B, strideB1, offsetB );
		}
	} else if ( N >= mnthr && lwork >= 4 * M + M * M + Math.max( M, 2 * M - 4, nrhs, N - 3 * M ) ) {
		// ==========================================
		// Path 2a: N >> M — LQ factorization with workspace copy
		// ==========================================
		ldwork = M;
		// Check if we can use a larger ldwork for better performance
		if ( lwork >= Math.max( 4 * M + M * M + Math.max( M, 2 * M - 4, nrhs, N - 3 * M ),
			M * M + M + M * nrhs ) ) {
			ldwork = Math.max( M, Math.floor( Math.sqrt( lwork - 3 * M ) ) );
			// Cap ldwork to a reasonable value
			if ( ldwork > M ) {
				ldwork = M; // keep simple — no LDA optimization needed in JS
			}
		}

		itau = 0;        // TAU starts at WORK[0], length M
		iwork = M;       // scratch after TAU

		// Compute A = L * Q
		dgelqf( M, N, A, strideA1, strideA2, offsetA,
			WORK, 1, itau,
			WORK, 1, iwork, lwork - iwork );

		il = iwork; // IL: copy of L starts here, ldwork-by-M stored column-major

		// Copy L into workspace and zero it in A
		dlacpy( 'L', M, M,
			A, strideA1, strideA2, offsetA,
			WORK, 1, ldwork, il );
		dlaset( 'U', M - 1, M - 1, 0.0, 0.0,
			WORK, 1, ldwork, il + ldwork );

		ie = il + ldwork * M;
		itauq = ie + M;
		itaup = itauq + M;
		iwork = itaup + M;

		// Bidiagonal reduction of L (M-by-M)
		dgebrd( M, M, WORK, 1, ldwork, il,
			S, strideS, offsetS,
			WORK, 1, ie,
			WORK, 1, itauq,
			WORK, 1, itaup,
			WORK, 1, iwork, lwork - iwork );

		// Multiply B by transpose of left bidiagonal transformation: B := Q_b^T * B
		dormbr( 'Q', 'L', 'T', M, nrhs, M,
			WORK, 1, ldwork, il,
			WORK, 1, itauq,
			B, strideB1, strideB2, offsetB,
			WORK, 1, iwork, lwork - iwork );

		// Generate right bidiagonal transformation of L
		dorgbr( 'P', M, M, M,
			WORK, 1, ldwork, il,
			WORK, 1, itaup,
			WORK, 1, iwork, lwork - iwork );
		iwork = ie + M;

		// Compute SVD of bidiagonal of L
		info = dbdsqr( 'U', M, M, 0, nrhs,
			S, strideS, offsetS,
			WORK, 1, ie,
			WORK, 1, ldwork, il,
			DUM, 1, 1, 0,
			B, strideB1, strideB2, offsetB,
			WORK, 1, iwork );
		if ( info !== 0 ) {
			rank[ 0 ] = 0;
			return info;
		}

		// Threshold for rank determination
		thr = Math.max( rcond * S[ offsetS ], sfmin );
		if ( rcond < 0.0 ) {
			thr = Math.max( eps * S[ offsetS ], sfmin );
		}
		rank[ 0 ] = 0;
		for ( i = 0; i < M; i++ ) {
			if ( S[ offsetS + i * strideS ] > thr ) {
				drscl( nrhs, S[ offsetS + i * strideS ],
					B, strideB2, offsetB + i * strideB1 );
				rank[ 0 ] += 1;
			} else {
				dlaset( 'F', 1, nrhs, 0.0, 0.0,
					B, strideB1, strideB2, offsetB + i * strideB1 );
			}
		}
		iwork = ie;

		// Multiply by right singular vectors of L: X_L = V_L^T * (Sigma^+ * ...)
		if ( lwork >= iwork + M * nrhs && nrhs > 1 ) {
			dgemm( 'T', 'N', M, nrhs, M, 1.0,
				WORK, 1, ldwork, il,
				B, strideB1, strideB2, offsetB,
				0.0, WORK, 1, M, iwork );
			dlacpy( 'G', M, nrhs,
				WORK, 1, M, iwork,
				B, strideB1, strideB2, offsetB );
		} else if ( nrhs > 1 ) {
			chunk = Math.max( 1, Math.floor( ( lwork - iwork ) / M ) );
			for ( i = 0; i < nrhs; i += chunk ) {
				bl = Math.min( nrhs - i, chunk );
				dgemm( 'T', 'N', M, bl, M, 1.0,
					WORK, 1, ldwork, il,
					B, strideB1, strideB2, offsetB + i * strideB2,
					0.0, WORK, 1, M, iwork );
				dlacpy( 'G', M, bl,
					WORK, 1, M, iwork,
					B, strideB1, strideB2, offsetB + i * strideB2 );
			}
		} else if ( nrhs === 1 ) {
			dgemv( 'T', M, M, 1.0,
				WORK, 1, ldwork, il,
				B, strideB1, offsetB,
				0.0, WORK, 1, iwork );
			dcopy( M, WORK, 1, iwork, B, strideB1, offsetB );
		}

		// Zero out B(M+1:N, 1:NRHS)
		dlaset( 'F', N - M, nrhs, 0.0, 0.0,
			B, strideB1, strideB2, offsetB + M * strideB1 );

		// Multiply by Q^T from LQ factorization: X = Q^T * [X_L; 0]
		iwork = itau + M;
		dormlq( 'L', 'T', N, nrhs, M,
			A, strideA1, strideA2, offsetA,
			WORK, 1, itau,
			B, strideB1, strideB2, offsetB,
			WORK, 1, iwork, lwork - iwork );
	} else {
		// ==========================================
		// Path 2b: N > M, not enough workspace for LQ+copy — direct bidiag
		// ==========================================
		ie = 0;
		itauq = ie + M;
		itaup = itauq + M;
		iwork = itaup + M;

		// Bidiagonal reduction of A (M-by-N)
		dgebrd( M, N, A, strideA1, strideA2, offsetA,
			S, strideS, offsetS,
			WORK, 1, ie,
			WORK, 1, itauq,
			WORK, 1, itaup,
			WORK, 1, iwork, lwork - iwork );

		// Multiply B by transpose of left bidiagonal transformation
		dormbr( 'Q', 'L', 'T', M, nrhs, N,
			A, strideA1, strideA2, offsetA,
			WORK, 1, itauq,
			B, strideB1, strideB2, offsetB,
			WORK, 1, iwork, lwork - iwork );

		// Generate right bidiagonal transformation
		dorgbr( 'P', M, N, M,
			A, strideA1, strideA2, offsetA,
			WORK, 1, itaup,
			WORK, 1, iwork, lwork - iwork );
		iwork = ie + M;

		// Compute SVD of bidiagonal (lower bidiagonal for M < N)
		info = dbdsqr( 'L', M, N, 0, nrhs,
			S, strideS, offsetS,
			WORK, 1, ie,
			A, strideA1, strideA2, offsetA,
			DUM, 1, 1, 0,
			B, strideB1, strideB2, offsetB,
			WORK, 1, iwork );
		if ( info !== 0 ) {
			rank[ 0 ] = 0;
			return info;
		}

		// Threshold for rank determination
		thr = Math.max( rcond * S[ offsetS ], sfmin );
		if ( rcond < 0.0 ) {
			thr = Math.max( eps * S[ offsetS ], sfmin );
		}
		rank[ 0 ] = 0;
		for ( i = 0; i < M; i++ ) {
			if ( S[ offsetS + i * strideS ] > thr ) {
				drscl( nrhs, S[ offsetS + i * strideS ],
					B, strideB2, offsetB + i * strideB1 );
				rank[ 0 ] += 1;
			} else {
				dlaset( 'F', 1, nrhs, 0.0, 0.0,
					B, strideB1, strideB2, offsetB + i * strideB1 );
			}
		}

		// Multiply by right singular vectors: X = V^T * (Sigma^+ * ...)
		if ( lwork >= strideB2 * nrhs && nrhs > 1 ) {
			dgemm( 'T', 'N', N, nrhs, M, 1.0,
				A, strideA1, strideA2, offsetA,
				B, strideB1, strideB2, offsetB,
				0.0, WORK, 1, N, 0 );
			dlacpy( 'F', N, nrhs,
				WORK, 1, N, 0,
				B, strideB1, strideB2, offsetB );
		} else if ( nrhs > 1 ) {
			chunk = Math.max( 1, Math.floor( lwork / N ) );
			for ( i = 0; i < nrhs; i += chunk ) {
				bl = Math.min( nrhs - i, chunk );
				dgemm( 'T', 'N', N, bl, M, 1.0,
					A, strideA1, strideA2, offsetA,
					B, strideB1, strideB2, offsetB + i * strideB2,
					0.0, WORK, 1, N, 0 );
				dlacpy( 'F', N, bl,
					WORK, 1, N, 0,
					B, strideB1, strideB2, offsetB + i * strideB2 );
			}
		} else if ( nrhs === 1 ) {
			dgemv( 'T', M, N, 1.0,
				A, strideA1, strideA2, offsetA,
				B, strideB1, offsetB,
				0.0, WORK, 1, 0 );
			dcopy( N, WORK, 1, 0, B, strideB1, offsetB );
		}
	}

	// Undo scaling
	if ( iascl === 1 ) {
		dlascl( 'G', 0, 0, anrm, smlnum, N, nrhs, B, strideB1, strideB2, offsetB );
		dlascl( 'G', 0, 0, smlnum, anrm, minmn, 1, S, strideS, 1, offsetS );
	} else if ( iascl === 2 ) {
		dlascl( 'G', 0, 0, anrm, bignum, N, nrhs, B, strideB1, strideB2, offsetB );
		dlascl( 'G', 0, 0, bignum, anrm, minmn, 1, S, strideS, 1, offsetS );
	}
	if ( ibscl === 1 ) {
		dlascl( 'G', 0, 0, smlnum, bnrm, N, nrhs, B, strideB1, strideB2, offsetB );
	} else if ( ibscl === 2 ) {
		dlascl( 'G', 0, 0, bignum, bnrm, N, nrhs, B, strideB1, strideB2, offsetB );
	}

	return 0;
}

/**
* Computes the workspace size needed for dgelss.
*
* @private
* @param {NonNegativeInteger} M - number of rows
* @param {NonNegativeInteger} N - number of columns
* @param {NonNegativeInteger} nrhs - number of right hand sides
* @param {NonNegativeInteger} mnthr - threshold for switching paths
* @returns {integer} workspace size
*/
function computeWorkSize( M, N, nrhs, mnthr ) {
	var bdspac;
	var minmn;
	var wsize;
	var mm;

	minmn = Math.min( M, N );
	if ( minmn === 0 ) {
		return 1;
	}

	// Generous workspace allocation covering all code paths
	mm = M;
	wsize = 1;
	bdspac = Math.max( 1, 5 * minmn );

	if ( M >= N ) {
		if ( M >= mnthr ) {
			mm = N;
			// QR path workspace: N (TAU) + max(N*NB for QR, N*NB for ORMQR)
			wsize = Math.max( wsize, N + N * NB );
		}
		// Bidiag workspace: 3*N + max(mm*NB, N*NB)
		wsize = Math.max( wsize, 3 * N + Math.max( mm, N ) * NB );
		wsize = Math.max( wsize, 3 * N + nrhs );
		wsize = Math.max( wsize, bdspac );
		wsize = Math.max( wsize, N * nrhs );
	} else {
		bdspac = Math.max( 1, 5 * M );
		if ( N >= mnthr ) {
			// LQ path with workspace copy
			wsize = M + M * NB; // LQ
			wsize = Math.max( wsize, M * M + 4 * M + M * NB ); // bidiag of L
			wsize = Math.max( wsize, M * M + M + bdspac ); // BDSQR
			wsize = Math.max( wsize, M * M + M + M * nrhs ); // GEMM
			wsize = Math.max( wsize, M + N * NB ); // ORMLQ
		} else {
			// Direct bidiag
			wsize = Math.max( wsize, 3 * M + N * NB );
			wsize = Math.max( wsize, 3 * M + nrhs );
			wsize = Math.max( wsize, bdspac );
			wsize = Math.max( wsize, N * nrhs );
		}
	}

	return wsize;
}


// EXPORTS //

module.exports = dgelss;
