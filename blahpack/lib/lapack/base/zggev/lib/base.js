'use strict';

// MODULES //

var dlamch = require( '../../dlamch/lib/base.js' );
var zlange = require( '../../zlange/lib/base.js' );
var zlascl = require( '../../zlascl/lib/base.js' );
var zggbal = require( '../../zggbal/lib/base.js' );
var zgeqrf = require( '../../zgeqrf/lib/base.js' );
var zgghrd = require( '../../zgghrd/lib/base.js' );
var zhgeqz = require( '../../zhgeqz/lib/base.js' );
var zggbak = require( '../../zggbak/lib/base.js' );
var zlaset = require( '../../zlaset/lib/base.js' );
var zunmqr = require( './zunmqr.js' );
var zungqr = require( './zungqr.js' );
var ztgevc = require( './ztgevc.js' );
var zlacpy = require( './zlacpy.js' );

// VARIABLES //

var ZERO = 0.0;
var ONE = 1.0;
var CZERO = new Float64Array( [ 0.0, 0.0 ] );
var CONE = new Float64Array( [ 1.0, 0.0 ] );

// FUNCTIONS //

/**
* ABS1: |re| + |im| (cheap complex absolute value).
*
* @private
* @param {Float64Array} arr - interleaved complex array
* @param {integer} idx - index of real part
* @returns {number} |re| + |im|
*/
function abs1( arr, idx ) {
	return Math.abs( arr[ idx ] ) + Math.abs( arr[ idx + 1 ] );
}

// MAIN //

/**
* Compute the generalized eigenvalues and optionally the left and/or
* right generalized eigenvectors of a complex matrix pair (A, B).
*
* A generalized eigenvalue for a pair of matrices (A,B) is a scalar
* lambda or a ratio alpha/beta = lambda, such that A - lambda*B is
* singular.
*
* Complex elements are stored as interleaved real/imaginary pairs.
* Element (i, j) has real part at `offset + i*stride1 + j*stride2`
* and imaginary part at `offset + i*stride1 + j*stride2 + 1`.
*
* WORK and RWORK are allocated internally; the caller does not need
* to provide them.
*
* @private
* @param {string} jobvl - 'N' for no left eigenvectors, 'V' for compute
* @param {string} jobvr - 'N' for no right eigenvectors, 'V' for compute
* @param {NonNegativeInteger} N - order of matrices A and B
* @param {Float64Array} A - first complex matrix (interleaved, modified in-place)
* @param {integer} strideA1 - stride of the first dimension of A
* @param {integer} strideA2 - stride of the second dimension of A
* @param {NonNegativeInteger} offsetA - starting index for A
* @param {Float64Array} B - second complex matrix (interleaved, modified in-place)
* @param {integer} strideB1 - stride of the first dimension of B
* @param {integer} strideB2 - stride of the second dimension of B
* @param {NonNegativeInteger} offsetB - starting index for B
* @param {Float64Array} ALPHA - output eigenvalue numerators (interleaved complex, length >= 2*N)
* @param {integer} strideALPHA - stride for ALPHA (complex elements)
* @param {NonNegativeInteger} offsetALPHA - starting index for ALPHA
* @param {Float64Array} BETA - output eigenvalue denominators (interleaved complex, length >= 2*N)
* @param {integer} strideBETA - stride for BETA (complex elements)
* @param {NonNegativeInteger} offsetBETA - starting index for BETA
* @param {Float64Array} VL - left eigenvector matrix (interleaved complex)
* @param {integer} strideVL1 - stride of the first dimension of VL
* @param {integer} strideVL2 - stride of the second dimension of VL
* @param {NonNegativeInteger} offsetVL - starting index for VL
* @param {Float64Array} VR - right eigenvector matrix (interleaved complex)
* @param {integer} strideVR1 - stride of the first dimension of VR
* @param {integer} strideVR2 - stride of the second dimension of VR
* @param {NonNegativeInteger} offsetVR - starting index for VR
* @returns {integer} INFO: 0=success, 1..N=QZ iteration failed to converge, N+1=other QZ failure, N+2=ZTGEVC error
*/
function zggev( jobvl, jobvr, N, A, strideA1, strideA2, offsetA, B, strideB1, strideB2, offsetB, ALPHA, strideALPHA, offsetALPHA, BETA, strideBETA, offsetBETA, VL, strideVL1, strideVL2, offsetVL, VR, strideVR1, strideVR2, offsetVR ) { // eslint-disable-line max-len, max-params
	var anrmto;
	var bnrmto;
	var ilascl;
	var ilbscl;
	var smlnum;
	var bignum;
	var chtemp;
	var iright;
	var ileft;
	var irwrk;
	var irows;
	var icols;
	var RWORK;
	var anrm;
	var bnrm;
	var ilvl;
	var ilvr;
	var ilv;
	var info;
	var ierr;
	var temp;
	var WORK;
	var lwork;
	var sA1;
	var sA2;
	var sB1;
	var sB2;
	var oA;
	var oB;
	var bal;
	var ilo;
	var ihi;
	var eps;
	var jc;
	var jr;

	// Decode input
	ilvl = ( jobvl === 'V' || jobvl === 'v' );
	ilvr = ( jobvr === 'V' || jobvr === 'v' );
	ilv = ilvl || ilvr;

	info = 0;

	// Quick return if possible
	if ( N === 0 ) {
		return info;
	}

	// Local aliases for strides and offsets
	sA1 = strideA1;
	sA2 = strideA2;
	sB1 = strideB1;
	sB2 = strideB2;
	oA = offsetA;
	oB = offsetB;

	// Allocate workspace
	// WORK: complex workspace (interleaved), at least 2*N complex elements = 4*N doubles
	// We need enough for zgeqrf, zunmqr, zhgeqz, ztgevc
	lwork = Math.max( 1, 8 * N );
	WORK = new Float64Array( 2 * lwork );

	// RWORK: real workspace, at least 8*N for zggbal(6N) + zhgeqz/ztgevc(2N)
	RWORK = new Float64Array( 8 * N );

	// Get machine constants
	eps = dlamch( 'E' ) * dlamch( 'B' );
	smlnum = dlamch( 'S' );
	bignum = ONE / smlnum;
	smlnum = Math.sqrt( smlnum ) / eps;
	bignum = ONE / smlnum;

	// Scale A if max element outside range [SMLNUM, BIGNUM]
	// zlange and zlascl expect complex-element strides (divide by 2)
	anrm = zlange( 'M', N, N, A, sA1 / 2, sA2 / 2, oA, RWORK, 1, 0 );
	ilascl = false;
	anrmto = 0.0;
	if ( anrm > ZERO && anrm < smlnum ) {
		anrmto = smlnum;
		ilascl = true;
	} else if ( anrm > bignum ) {
		anrmto = bignum;
		ilascl = true;
	}
	if ( ilascl ) {
		zlascl( 'G', 0, 0, anrm, anrmto, N, N, A, sA1 / 2, sA2 / 2, oA );
	}

	// Scale B if max element outside range [SMLNUM, BIGNUM]
	bnrm = zlange( 'M', N, N, B, sB1 / 2, sB2 / 2, oB, RWORK, 1, 0 );
	ilbscl = false;
	bnrmto = 0.0;
	if ( bnrm > ZERO && bnrm < smlnum ) {
		bnrmto = smlnum;
		ilbscl = true;
	} else if ( bnrm > bignum ) {
		bnrmto = bignum;
		ilbscl = true;
	}
	if ( ilbscl ) {
		zlascl( 'G', 0, 0, bnrm, bnrmto, N, N, B, sB1 / 2, sB2 / 2, oB );
	}

	// Permute the matrices A, B to isolate eigenvalues
	// RWORK layout: LSCALE[0..N-1], RSCALE[N..2N-1], work[2N..8N-1]
	ileft = 0;
	iright = N;
	irwrk = 2 * N;
	bal = zggbal( 'P', N, A, sA1, sA2, oA, B, sB1, sB2, oB,
		RWORK, 1, ileft, RWORK, 1, iright, RWORK, 1, irwrk );
	ilo = bal.ilo;
	ihi = bal.ihi;

	// Compute active block dimensions (1-based ilo, ihi)
	irows = ihi - ilo + 1;
	if ( ilv ) {
		icols = N - ilo + 1;
	} else {
		icols = irows;
	}

	// QR factorize B(ilo:ihi, ilo:icols) using ZGEQRF
	// zgeqrf uses complex-element strides, so divide by 2
	// TAU goes at WORK[0..], scratch at WORK[irows*2..]
	zgeqrf(
		irows, icols,
		B, sB1 / 2, sB2 / 2, oB + ( ilo - 1 ) * sB1 + ( ilo - 1 ) * sB2,
		WORK, 1, 0,
		WORK, 1, irows * 2
	);

	// Apply Q^H to A from the left: A(ilo:ihi, ilo:icols) := Q^H * A(ilo:ihi, ilo:icols)
	zunmqr(
		'L', 'C', irows, icols, irows,
		B, sB1, sB2, oB + ( ilo - 1 ) * sB1 + ( ilo - 1 ) * sB2,
		WORK, 1, 0,
		A, sA1, sA2, oA + ( ilo - 1 ) * sA1 + ( ilo - 1 ) * sA2,
		WORK, 1, irows * 2,
		WORK, lwork - irows
	);

	// Initialize VL and generate Q
	if ( ilvl ) {
		zlaset( 'Full', N, N, CZERO, CONE, VL, strideVL1, strideVL2, offsetVL );

		if ( irows > 1 ) {
			// Copy lower triangular part of B(ilo+1:ihi, ilo:ilo+irows-2) to VL
			zlacpy( 'L', irows - 1, irows - 1,
				B, sB1, sB2, oB + ilo * sB1 + ( ilo - 1 ) * sB2,
				VL, strideVL1, strideVL2, offsetVL + ilo * strideVL1 + ( ilo - 1 ) * strideVL2
			);
		}

		// Generate Q from Householder reflectors
		zungqr( irows, irows, irows,
			VL, strideVL1, strideVL2, offsetVL + ( ilo - 1 ) * strideVL1 + ( ilo - 1 ) * strideVL2,
			WORK, 1, 0,
			WORK, irows * 2
		);
	}

	// Initialize VR
	if ( ilvr ) {
		zlaset( 'Full', N, N, CZERO, CONE, VR, strideVR1, strideVR2, offsetVR );
	}

	// Reduce to generalized Hessenberg form
	if ( ilv ) {
		// Eigenvectors requested - work on whole matrix
		zgghrd(
			jobvl, jobvr, N, ilo, ihi,
			A, sA1, sA2, oA,
			B, sB1, sB2, oB,
			VL, strideVL1, strideVL2, offsetVL,
			VR, strideVR1, strideVR2, offsetVR
		);
	} else {
		// No eigenvectors - work on active block only
		zgghrd(
			'N', 'N', irows, 1, irows,
			A, sA1, sA2, oA + ( ilo - 1 ) * sA1 + ( ilo - 1 ) * sA2,
			B, sB1, sB2, oB + ( ilo - 1 ) * sB1 + ( ilo - 1 ) * sB2,
			VL, strideVL1, strideVL2, offsetVL,
			VR, strideVR1, strideVR2, offsetVR
		);
	}

	// QZ algorithm: compute eigenvalues and optionally Schur form
	if ( ilv ) {
		chtemp = 'S';
	} else {
		chtemp = 'E';
	}
	ierr = zhgeqz(
		chtemp, jobvl, jobvr, N, ilo, ihi,
		A, sA1, sA2, oA,
		B, sB1, sB2, oB,
		ALPHA, strideALPHA * 2, offsetALPHA,
		BETA, strideBETA * 2, offsetBETA,
		VL, strideVL1, strideVL2, offsetVL,
		VR, strideVR1, strideVR2, offsetVR,
		WORK, 1, 0, lwork,
		RWORK, 1, irwrk
	);
	if ( ierr !== 0 ) {
		if ( ierr > 0 && ierr <= N ) {
			info = ierr;
		} else if ( ierr > N && ierr <= 2 * N ) {
			info = ierr - N;
		} else {
			info = N + 1;
		}
		// Jump to unscaling (label 70)
		return finalize();
	}

	// Compute eigenvectors
	if ( ilv ) {
		if ( ilvl ) {
			if ( ilvr ) {
				chtemp = 'B';
			} else {
				chtemp = 'L';
			}
		} else {
			chtemp = 'R';
		}

		ierr = ztgevc(
			chtemp, 'B', N,
			A, sA1, sA2, oA,
			B, sB1, sB2, oB,
			VL, strideVL1, strideVL2, offsetVL,
			VR, strideVR1, strideVR2, offsetVR,
			N, WORK, RWORK
		);
		if ( ierr !== 0 ) {
			info = N + 2;
			return finalize();
		}

		// Undo balancing on VL and VR, and normalize
		if ( ilvl ) {
			zggbak( 'P', 'L', N, ilo, ihi,
				RWORK, 1, ileft, RWORK, 1, iright,
				N, VL, strideVL1, strideVL2, offsetVL
			);

			// Normalize left eigenvectors
			for ( jc = 0; jc < N; jc++ ) {
				temp = ZERO;
				for ( jr = 0; jr < N; jr++ ) {
					temp = Math.max( temp, abs1( VL, offsetVL + jr * strideVL1 + jc * strideVL2 ) );
				}
				if ( temp < smlnum ) {
					continue;
				}
				temp = ONE / temp;
				for ( jr = 0; jr < N; jr++ ) {
					VL[ offsetVL + jr * strideVL1 + jc * strideVL2 ] *= temp;
					VL[ offsetVL + jr * strideVL1 + jc * strideVL2 + 1 ] *= temp;
				}
			}
		}

		if ( ilvr ) {
			zggbak( 'P', 'R', N, ilo, ihi,
				RWORK, 1, ileft, RWORK, 1, iright,
				N, VR, strideVR1, strideVR2, offsetVR
			);

			// Normalize right eigenvectors
			for ( jc = 0; jc < N; jc++ ) {
				temp = ZERO;
				for ( jr = 0; jr < N; jr++ ) {
					temp = Math.max( temp, abs1( VR, offsetVR + jr * strideVR1 + jc * strideVR2 ) );
				}
				if ( temp < smlnum ) {
					continue;
				}
				temp = ONE / temp;
				for ( jr = 0; jr < N; jr++ ) {
					VR[ offsetVR + jr * strideVR1 + jc * strideVR2 ] *= temp;
					VR[ offsetVR + jr * strideVR1 + jc * strideVR2 + 1 ] *= temp;
				}
			}
		}
	}

	return finalize();

	/**
	* Undo scaling on ALPHA and BETA if necessary, then return info.
	*
	* @private
	* @returns {integer} info
	*/
	function finalize() {
		if ( ilascl ) {
			// ALPHA is treated as an N-by-1 complex matrix for zlascl
			// zlascl expects complex-element strides
			zlascl( 'G', 0, 0, anrmto, anrm, N, 1,
				ALPHA, strideALPHA, 1, offsetALPHA
			);
		}
		if ( ilbscl ) {
			zlascl( 'G', 0, 0, bnrmto, bnrm, N, 1,
				BETA, strideBETA, 1, offsetBETA
			);
		}
		return info;
	}
}


// EXPORTS //

module.exports = zggev;
