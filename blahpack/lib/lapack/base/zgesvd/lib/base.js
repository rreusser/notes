/**
* @license Apache-2.0
*
* Copyright (c) 2025 Ricky Reusser.
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
*    http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/

'use strict';

// MODULES //

var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var Complex128Array = require( '@stdlib/array/complex128' );
var dlamch = require( '../../dlamch/lib/base.js' );
var dlascl = require( '../../dlascl/lib/base.js' );
var zbdsqr = require( '../../zbdsqr/lib/base.js' );
var zgebrd = require( '../../zgebrd/lib/base.js' );
var zgelqf = require( '../../zgelqf/lib/base.js' );
var zgeqrf = require( '../../zgeqrf/lib/base.js' );
var zgemm = require( '../../../../blas/base/zgemm/lib/base.js' );
var zlacpy = require( '../../zlacpy/lib/base.js' );
var zlange = require( '../../zlange/lib/base.js' );
var zlascl = require( '../../zlascl/lib/base.js' );
var zlaset = require( '../../zlaset/lib/base.js' );
var zlarf = require( '../../zlarf/lib/base.js' );
var zlarfb = require( '../../zlarfb/lib/base.js' );
var zlarft = require( '../../zlarft/lib/base.js' );
var zlacgv = require( '../../zlacgv/lib/base.js' );
var zscal = require( '../../../../blas/base/zscal/lib/base.js' );


// VARIABLES //

var CZERO = new Float64Array( [ 0.0, 0.0 ] );
var CONE = new Float64Array( [ 1.0, 0.0 ] );
var DEFAULT_NB = 32;


// FUNCTIONS //

/**
* ZUNG2R generates an M-by-N complex matrix Q with orthonormal columns,
* which is defined as the first N columns of a product of K elementary
* reflectors of order M:  Q = H(1) H(2) ... H(k)
*
* Unblocked algorithm using zlarf.
*
* @private
*/
function zung2r( M, N, K, A, sa1, sa2, offsetA, TAU, strideTAU, offsetTAU, WORK, strideWORK, offsetWORK ) { // eslint-disable-line max-len, max-params
	var negTau;
	var idx;
	var i;
	var j;
	var l;

	if ( N <= 0 ) {
		return 0;
	}

	negTau = new Float64Array( 2 );

	// Initialize columns K+1:N to columns of the unit matrix
	for ( j = K; j < N; j++ ) {
		for ( l = 0; l < M; l++ ) {
			idx = offsetA + 2 * ( l * sa1 + j * sa2 );
			A[ idx ] = 0.0;
			A[ idx + 1 ] = 0.0;
		}
		idx = offsetA + 2 * ( j * sa1 + j * sa2 );
		A[ idx ] = 1.0;
		A[ idx + 1 ] = 0.0;
	}

	for ( i = K - 1; i >= 0; i-- ) {
		// Apply H(i) to A(i:m-1, i:n-1) from the left
		if ( i < N - 1 ) {
			idx = offsetA + 2 * ( i * sa1 + i * sa2 );
			A[ idx ] = 1.0;
			A[ idx + 1 ] = 0.0;
			zlarf(
				'L', M - i, N - i - 1,
				A, sa1, offsetA + 2 * ( i * sa1 + i * sa2 ),
				TAU, offsetTAU + 2 * i * strideTAU,
				A, sa1, sa2, offsetA + 2 * ( i * sa1 + ( i + 1 ) * sa2 ),
				WORK, strideWORK, offsetWORK
			);
		}
		if ( i < M - 1 ) {
			negTau[ 0 ] = -TAU[ offsetTAU + 2 * i * strideTAU ];
			negTau[ 1 ] = -TAU[ offsetTAU + 2 * i * strideTAU + 1 ];
			zscal( M - i - 1, negTau, A, sa1, offsetA + 2 * ( ( i + 1 ) * sa1 + i * sa2 ) );
		}
		idx = offsetA + 2 * ( i * sa1 + i * sa2 );
		A[ idx ] = 1.0 - TAU[ offsetTAU + 2 * i * strideTAU ];
		A[ idx + 1 ] = -TAU[ offsetTAU + 2 * i * strideTAU + 1 ];

		// Set A(0:i-1, i) to zero
		for ( l = 0; l < i; l++ ) {
			idx = offsetA + 2 * ( l * sa1 + i * sa2 );
			A[ idx ] = 0.0;
			A[ idx + 1 ] = 0.0;
		}
	}
	return 0;
}

/**
* ZUNGL2 generates an M-by-N complex matrix Q with orthonormal rows,
* which is defined as the first M rows of a product of K elementary
* reflectors of order N:  Q = H(k)^H ... H(2)^H H(1)^H
*
* Unblocked algorithm using zlarf.
*
* @private
*/
function zungl2( M, N, K, A, sa1, sa2, offsetA, TAU, strideTAU, offsetTAU, WORK, strideWORK, offsetWORK ) { // eslint-disable-line max-len, max-params
	var negTau;
	var conjTau;
	var idx;
	var tauR;
	var tauI;
	var i;
	var j;
	var l;

	if ( M <= 0 ) {
		return 0;
	}

	negTau = new Float64Array( 2 );
	conjTau = new Float64Array( 2 );

	// Initialize rows K+1:M to rows of the unit matrix
	if ( K < M ) {
		for ( j = 0; j < N; j++ ) {
			for ( l = K; l < M; l++ ) {
				idx = offsetA + 2 * ( l * sa1 + j * sa2 );
				A[ idx ] = 0.0;
				A[ idx + 1 ] = 0.0;
			}
			if ( j > K - 1 && j < M ) {
				idx = offsetA + 2 * ( j * sa1 + j * sa2 );
				A[ idx ] = 1.0;
				A[ idx + 1 ] = 0.0;
			}
		}
	}

	for ( i = K - 1; i >= 0; i-- ) {
		tauR = TAU[ offsetTAU + 2 * i * strideTAU ];
		tauI = TAU[ offsetTAU + 2 * i * strideTAU + 1 ];

		// Apply H(i)^H to A(i:m-1, i:n-1) from the right
		if ( i < N - 1 ) {
			// Conjugate row i from column i+1 onward
			zlacgv( N - i - 1, A, sa2, offsetA + 2 * ( i * sa1 + ( i + 1 ) * sa2 ) );
			if ( i < M - 1 ) {
				idx = offsetA + 2 * ( i * sa1 + i * sa2 );
				A[ idx ] = 1.0;
				A[ idx + 1 ] = 0.0;
				// Apply H(i)^H from the right: use conj(tau)
				conjTau[ 0 ] = tauR;
				conjTau[ 1 ] = -tauI;
				zlarf(
					'R', M - i - 1, N - i,
					A, sa2, offsetA + 2 * ( i * sa1 + i * sa2 ),
					conjTau, 0,
					A, sa1, sa2, offsetA + 2 * ( ( i + 1 ) * sa1 + i * sa2 ),
					WORK, strideWORK, offsetWORK
				);
			}
			negTau[ 0 ] = -tauR;
			negTau[ 1 ] = -tauI;
			zscal( N - i - 1, negTau, A, sa2, offsetA + 2 * ( i * sa1 + ( i + 1 ) * sa2 ) );
			// Conjugate back
			zlacgv( N - i - 1, A, sa2, offsetA + 2 * ( i * sa1 + ( i + 1 ) * sa2 ) );
		}
		idx = offsetA + 2 * ( i * sa1 + i * sa2 );
		A[ idx ] = 1.0 - tauR;
		A[ idx + 1 ] = tauI;

		// Set A(i, 0:i-1) to zero
		for ( l = 0; l < i; l++ ) {
			idx = offsetA + 2 * ( i * sa1 + l * sa2 );
			A[ idx ] = 0.0;
			A[ idx + 1 ] = 0.0;
		}
	}
	return 0;
}

/**
* ZUNGQR generates an M-by-N complex matrix Q with orthonormal columns,
* which is defined as the first N columns of a product of K elementary
* reflectors of order M:  Q = H(1) H(2) ... H(k)
*
* Blocked algorithm using zlarft + zlarfb.
*
* @private
*/
function zungqr( M, N, K, A, sa1, sa2, offsetA, TAU, strideTAU, offsetTAU, WORK, strideWORK, offsetWORK ) { // eslint-disable-line max-len, max-params
	var ldwork;
	var nb;
	var kk;
	var ki;
	var ib;
	var T;
	var i;
	var j;
	var l;
	var idx;

	if ( N <= 0 ) {
		return 0;
	}

	nb = DEFAULT_NB;
	T = new Float64Array( 2 * nb * nb );

	if ( nb >= 2 && nb < K ) {
		ki = ( Math.floor( ( K - 1 ) / nb ) ) * nb;
		kk = Math.min( K, ki + nb );

		// Set rows 0:kk-1 of columns kk:N-1 to zero
		for ( j = kk; j < N; j++ ) {
			for ( i = 0; i < kk; i++ ) {
				idx = offsetA + 2 * ( i * sa1 + j * sa2 );
				A[ idx ] = 0.0;
				A[ idx + 1 ] = 0.0;
			}
		}
	} else {
		kk = 0;
	}

	// Use unblocked code for the last or only block
	if ( kk < N ) {
		zung2r(
			M - kk, N - kk, K - kk,
			A, sa1, sa2, offsetA + 2 * ( kk * sa1 + kk * sa2 ),
			TAU, strideTAU, offsetTAU + 2 * kk * strideTAU,
			WORK, strideWORK, offsetWORK
		);
	}

	if ( kk > 0 ) {
		ldwork = N;

		// Ensure WORK is large enough
		if ( !WORK || WORK.length < 2 * ldwork * nb + offsetWORK ) {
			WORK = new Float64Array( 2 * ldwork * nb + offsetWORK );
		}

		for ( i = ki; i >= 0; i -= nb ) {
			ib = Math.min( nb, K - i );
			if ( i + ib < N ) {
				// Form the triangular factor of the block reflector
				zlarft(
					'F', 'C',
					M - i, ib,
					A, sa1, sa2, offsetA + 2 * ( i * sa1 + i * sa2 ),
					TAU, strideTAU, offsetTAU + 2 * i * strideTAU,
					T, 1, nb, 0
				);

				// Apply H to A(i:m-1, i+ib:n-1) from the left
				zlarfb(
					'L', 'N', 'F', 'C',
					M - i, N - i - ib, ib,
					A, sa1, sa2, offsetA + 2 * ( i * sa1 + i * sa2 ),
					T, 1, nb, 0,
					A, sa1, sa2, offsetA + 2 * ( i * sa1 + ( i + ib ) * sa2 ),
					WORK, 1, ldwork, offsetWORK
				);
			}

			// Apply H to rows i:m-1 of current block
			zung2r(
				M - i, ib, ib,
				A, sa1, sa2, offsetA + 2 * ( i * sa1 + i * sa2 ),
				TAU, strideTAU, offsetTAU + 2 * i * strideTAU,
				WORK, strideWORK, offsetWORK
			);

			// Set rows 0:i-1 of current block to zero
			for ( j = i; j < i + ib; j++ ) {
				for ( l = 0; l < i; l++ ) {
					idx = offsetA + 2 * ( l * sa1 + j * sa2 );
					A[ idx ] = 0.0;
					A[ idx + 1 ] = 0.0;
				}
			}
		}
	}
	return 0;
}

/**
* ZUNGLQ generates an M-by-N complex matrix Q with orthonormal rows,
* which is defined as the first M rows of a product of K elementary
* reflectors of order N:  Q = H(k)^H ... H(2)^H H(1)^H
*
* Blocked algorithm using zlarft + zlarfb.
*
* @private
*/
function zunglq( M, N, K, A, sa1, sa2, offsetA, TAU, strideTAU, offsetTAU, WORK, strideWORK, offsetWORK ) { // eslint-disable-line max-len, max-params
	var ldwork;
	var nb;
	var kk;
	var ki;
	var ib;
	var T;
	var i;
	var j;
	var l;
	var idx;

	if ( M <= 0 ) {
		return 0;
	}

	nb = DEFAULT_NB;
	T = new Float64Array( 2 * nb * nb );

	if ( nb >= 2 && nb < K ) {
		ki = ( Math.floor( ( K - 1 ) / nb ) ) * nb;
		kk = Math.min( K, ki + nb );

		// Set columns 0:kk-1 of rows kk:M-1 to zero
		for ( j = 0; j < kk; j++ ) {
			for ( i = kk; i < M; i++ ) {
				idx = offsetA + 2 * ( i * sa1 + j * sa2 );
				A[ idx ] = 0.0;
				A[ idx + 1 ] = 0.0;
			}
		}
	} else {
		kk = 0;
	}

	// Use unblocked code for the last or only block
	if ( kk < M ) {
		zungl2(
			M - kk, N - kk, K - kk,
			A, sa1, sa2, offsetA + 2 * ( kk * sa1 + kk * sa2 ),
			TAU, strideTAU, offsetTAU + 2 * kk * strideTAU,
			WORK, strideWORK, offsetWORK
		);
	}

	if ( kk > 0 ) {
		ldwork = M;

		// Ensure WORK is large enough
		if ( !WORK || WORK.length < 2 * ldwork * nb + offsetWORK ) {
			WORK = new Float64Array( 2 * ldwork * nb + offsetWORK );
		}

		for ( i = ki; i >= 0; i -= nb ) {
			ib = Math.min( nb, K - i );
			if ( i + ib < M ) {
				// Form the triangular factor of the block reflector
				zlarft(
					'F', 'R',
					N - i, ib,
					A, sa1, sa2, offsetA + 2 * ( i * sa1 + i * sa2 ),
					TAU, strideTAU, offsetTAU + 2 * i * strideTAU,
					T, 1, nb, 0
				);

				// Apply H^H to A(i+ib:m-1, i:n-1) from the right
				zlarfb(
					'R', 'C', 'F', 'R',
					M - i - ib, N - i, ib,
					A, sa1, sa2, offsetA + 2 * ( i * sa1 + i * sa2 ),
					T, 1, nb, 0,
					A, sa1, sa2, offsetA + 2 * ( ( i + ib ) * sa1 + i * sa2 ),
					WORK, 1, ldwork, offsetWORK
				);
			}

			// Apply H to rows i:i+ib-1 of current block
			zungl2(
				ib, N - i, ib,
				A, sa1, sa2, offsetA + 2 * ( i * sa1 + i * sa2 ),
				TAU, strideTAU, offsetTAU + 2 * i * strideTAU,
				WORK, strideWORK, offsetWORK
			);

			// Set columns 0:i-1 of rows i:i+ib-1 to zero
			for ( j = 0; j < i; j++ ) {
				for ( l = i; l < i + ib; l++ ) {
					idx = offsetA + 2 * ( l * sa1 + j * sa2 );
					A[ idx ] = 0.0;
					A[ idx + 1 ] = 0.0;
				}
			}
		}
	}
	return 0;
}

/**
* ZUNGBR generates one of the complex unitary matrices Q or P^H
* determined by ZGEBRD when reducing a complex matrix A to bidiagonal form.
*
* If VECT = 'Q', generates the M-by-N matrix Q (left unitary factor).
* If VECT = 'P', generates the M-by-N matrix P^H (right unitary factor).
*
* @private
*/
function zungbr( vect, M, N, K, A, sa1, sa2, offsetA, TAU, strideTAU, offsetTAU, WORK, strideWORK, offsetWORK ) { // eslint-disable-line max-len, max-params
	var wantq;
	var idx;
	var i;
	var j;

	wantq = ( vect === 'Q' || vect === 'q' );

	if ( M === 0 || N === 0 ) {
		return 0;
	}

	if ( wantq ) {
		// Form Q: requires M >= N >= min(M, K)
		if ( M >= K ) {
			// Q was determined by a call to zgebrd with M >= N
			zungqr( M, N, K, A, sa1, sa2, offsetA, TAU, strideTAU, offsetTAU, WORK, strideWORK, offsetWORK );
		} else {
			// Q was determined by a call to zgebrd with M < N
			// Shift columns right to make room, insert identity in first row/col
			for ( j = M - 1; j >= 1; j-- ) {
				idx = offsetA + 2 * ( 0 * sa1 + j * sa2 );
				A[ idx ] = 0.0;
				A[ idx + 1 ] = 0.0;
				for ( i = j + 1; i < M; i++ ) {
					idx = offsetA + 2 * ( i * sa1 + j * sa2 );
					A[ idx ] = A[ offsetA + 2 * ( i * sa1 + ( j - 1 ) * sa2 ) ];
					A[ idx + 1 ] = A[ offsetA + 2 * ( i * sa1 + ( j - 1 ) * sa2 ) + 1 ];
				}
			}
			idx = offsetA;
			A[ idx ] = 1.0;
			A[ idx + 1 ] = 0.0;
			for ( i = 1; i < M; i++ ) {
				idx = offsetA + 2 * ( i * sa1 );
				A[ idx ] = 0.0;
				A[ idx + 1 ] = 0.0;
			}
			if ( M > 1 ) {
				zungqr(
					M - 1, M - 1, M - 1,
					A, sa1, sa2, offsetA + 2 * ( 1 * sa1 + 1 * sa2 ),
					TAU, strideTAU, offsetTAU,
					WORK, strideWORK, offsetWORK
				);
			}
		}
	} else {
		// Form P^H: requires N >= M >= min(N, K)
		if ( K < N ) {
			// P^H was determined by a call to zgebrd with M >= N
			zunglq( M, N, K, A, sa1, sa2, offsetA, TAU, strideTAU, offsetTAU, WORK, strideWORK, offsetWORK );
		} else {
			// P^H was determined by a call to zgebrd with M < N
			// Shift rows down to make room, insert identity in first row/col
			idx = offsetA;
			A[ idx ] = 1.0;
			A[ idx + 1 ] = 0.0;
			for ( i = 1; i < N; i++ ) {
				idx = offsetA + 2 * ( i * sa1 );
				A[ idx ] = 0.0;
				A[ idx + 1 ] = 0.0;
			}
			for ( j = 1; j < N; j++ ) {
				for ( i = j - 1; i >= 1; i-- ) {
					idx = offsetA + 2 * ( i * sa1 + j * sa2 );
					A[ idx ] = A[ offsetA + 2 * ( ( i - 1 ) * sa1 + j * sa2 ) ];
					A[ idx + 1 ] = A[ offsetA + 2 * ( ( i - 1 ) * sa1 + j * sa2 ) + 1 ];
				}
				idx = offsetA + 2 * ( 0 * sa1 + j * sa2 );
				A[ idx ] = 0.0;
				A[ idx + 1 ] = 0.0;
			}
			if ( N > 1 ) {
				zunglq(
					N - 1, N - 1, N - 1,
					A, sa1, sa2, offsetA + 2 * ( 1 * sa1 + 1 * sa2 ),
					TAU, strideTAU, offsetTAU,
					WORK, strideWORK, offsetWORK
				);
			}
		}
	}
	return 0;
}

/**
* ZUNMQR applies Q or Q^H from QR factorization to a matrix C.
*
* Q is represented as a product of elementary reflectors H(1)...H(k).
* This is the blocked version using zlarft + zlarfb, with fallback to
* unblocked (applying each reflector individually via zlarf).
*
* @private
*/
function zunmqr( side, trans, M, N, K, A, sa1A, sa2A, offsetA, TAU, strideTAU, offsetTAU, C, sc1, sc2, offsetC, WORK, strideWORK, offsetWORK ) { // eslint-disable-line max-len, max-params
	var ldwork;
	var left;
	var notran;
	var nb;
	var nw;
	var ib;
	var ic;
	var jc;
	var mi;
	var ni;
	var T;
	var i;
	var i1;
	var i2;
	var i3;

	left = ( side === 'L' || side === 'l' );
	notran = ( trans === 'N' || trans === 'n' );

	if ( M === 0 || N === 0 || K === 0 ) {
		return 0;
	}

	nb = DEFAULT_NB;
	T = new Float64Array( 2 * nb * nb );

	nw = left ? N : M;
	ldwork = nw;

	// Ensure WORK is large enough
	if ( !WORK || WORK.length < 2 * ldwork * nb + offsetWORK ) {
		WORK = new Float64Array( 2 * ldwork * nb + offsetWORK );
	}

	if ( ( left && !notran ) || ( !left && notran ) ) {
		i1 = 0;
		i2 = K;
		i3 = nb;
	} else {
		i1 = ( Math.floor( ( K - 1 ) / nb ) ) * nb;
		i2 = -1;
		i3 = -nb;
	}

	for ( i = i1; ( i3 > 0 ) ? ( i < i2 ) : ( i > i2 ); i += i3 ) {
		ib = Math.min( nb, K - i );

		// Form the triangular factor of the block reflector
		zlarft(
			'F', 'C',
			( left ? M - i : N - i ), ib,
			A, sa1A, sa2A, offsetA + 2 * ( i * sa1A + i * sa2A ),
			TAU, strideTAU, offsetTAU + 2 * i * strideTAU,
			T, 1, nb, 0
		);

		if ( left ) {
			mi = M - i;
			ni = N;
			ic = i;
			jc = 0;
		} else {
			mi = M;
			ni = N - i;
			ic = 0;
			jc = i;
		}

		// Apply H or H^H
		zlarfb(
			side, trans, 'F', 'C',
			mi, ni, ib,
			A, sa1A, sa2A, offsetA + 2 * ( i * sa1A + i * sa2A ),
			T, 1, nb, 0,
			C, sc1, sc2, offsetC + 2 * ( ic * sc1 + jc * sc2 ),
			WORK, 1, ldwork, offsetWORK
		);
	}
	return 0;
}

/**
* ZUNMLQ applies Q or Q^H from LQ factorization to a matrix C.
*
* Q is represented as a product of elementary reflectors H(1)^H...H(k)^H.
* This is the blocked version using zlarft + zlarfb.
*
* @private
*/
function zunmlq( side, trans, M, N, K, A, sa1A, sa2A, offsetA, TAU, strideTAU, offsetTAU, C, sc1, sc2, offsetC, WORK, strideWORK, offsetWORK ) { // eslint-disable-line max-len, max-params
	var ldwork;
	var left;
	var notran;
	var transt;
	var nb;
	var nw;
	var ib;
	var ic;
	var jc;
	var mi;
	var ni;
	var T;
	var i;
	var i1;
	var i2;
	var i3;

	left = ( side === 'L' || side === 'l' );
	notran = ( trans === 'N' || trans === 'n' );

	if ( M === 0 || N === 0 || K === 0 ) {
		return 0;
	}

	nb = DEFAULT_NB;
	T = new Float64Array( 2 * nb * nb );

	nw = left ? N : M;
	ldwork = nw;

	// Ensure WORK is large enough
	if ( !WORK || WORK.length < 2 * ldwork * nb + offsetWORK ) {
		WORK = new Float64Array( 2 * ldwork * nb + offsetWORK );
	}

	if ( ( left && notran ) || ( !left && !notran ) ) {
		i1 = 0;
		i2 = K;
		i3 = nb;
	} else {
		i1 = ( Math.floor( ( K - 1 ) / nb ) ) * nb;
		i2 = -1;
		i3 = -nb;
	}

	// For LQ reflectors, the transpose is flipped compared to QR
	transt = notran ? 'C' : 'N';

	for ( i = i1; ( i3 > 0 ) ? ( i < i2 ) : ( i > i2 ); i += i3 ) {
		ib = Math.min( nb, K - i );

		// Form the triangular factor of the block reflector
		zlarft(
			'F', 'R',
			( left ? M - i : N - i ), ib,
			A, sa1A, sa2A, offsetA + 2 * ( i * sa1A + i * sa2A ),
			TAU, strideTAU, offsetTAU + 2 * i * strideTAU,
			T, 1, nb, 0
		);

		if ( left ) {
			mi = M - i;
			ni = N;
			ic = i;
			jc = 0;
		} else {
			mi = M;
			ni = N - i;
			ic = 0;
			jc = i;
		}

		// Apply H or H^H
		zlarfb(
			side, transt, 'F', 'R',
			mi, ni, ib,
			A, sa1A, sa2A, offsetA + 2 * ( i * sa1A + i * sa2A ),
			T, 1, nb, 0,
			C, sc1, sc2, offsetC + 2 * ( ic * sc1 + jc * sc2 ),
			WORK, 1, ldwork, offsetWORK
		);
	}
	return 0;
}

/**
* ZUNMBR applies Q or P^H from bidiagonal reduction (ZGEBRD) to a matrix C.
*
* If VECT = 'Q': applies Q or Q^H (using ZUNMQR with column reflectors).
* If VECT = 'P': applies P or P^H (using ZUNMLQ with row reflectors).
*
* @private
*/
function zunmbr( vect, side, trans, M, N, K, A, sa1A, sa2A, offsetA, TAU, strideTAU, offsetTAU, C, sc1, sc2, offsetC, WORK, strideWORK, offsetWORK ) { // eslint-disable-line max-len, max-params
	var applyq;
	var left;
	var notran;
	var transt;
	var mi;
	var ni;
	var i1;
	var i2;
	var nq;

	applyq = ( vect === 'Q' || vect === 'q' );
	left = ( side === 'L' || side === 'l' );
	notran = ( trans === 'N' || trans === 'n' );

	if ( M === 0 || N === 0 ) {
		return 0;
	}

	nq = left ? M : N;

	if ( applyq ) {
		// Apply Q or Q^H
		if ( nq >= K ) {
			// Q was determined by zgebrd with NQ >= K
			zunmqr( side, trans, M, N, K, A, sa1A, sa2A, offsetA, TAU, strideTAU, offsetTAU, C, sc1, sc2, offsetC, WORK, strideWORK, offsetWORK );
		} else if ( nq > 1 ) {
			// Q was determined by zgebrd with NQ < K (M < N case)
			if ( left ) {
				mi = M - 1;
				ni = N;
				i1 = 1;
				i2 = 0;
			} else {
				mi = M;
				ni = N - 1;
				i1 = 0;
				i2 = 1;
			}
			zunmqr(
				side, trans, mi, ni, nq - 1,
				A, sa1A, sa2A, offsetA + 2 * ( 1 * sa1A + 0 * sa2A ),
				TAU, strideTAU, offsetTAU,
				C, sc1, sc2, offsetC + 2 * ( i1 * sc1 + i2 * sc2 ),
				WORK, strideWORK, offsetWORK
			);
		}
	} else {
		// Apply P or P^H (flip transpose)
		transt = notran ? 'C' : 'N';
		if ( nq > K ) {
			// P was determined by zgebrd with NQ > K
			zunmlq( side, transt, M, N, K, A, sa1A, sa2A, offsetA, TAU, strideTAU, offsetTAU, C, sc1, sc2, offsetC, WORK, strideWORK, offsetWORK );
		} else if ( nq > 1 ) {
			// P was determined by zgebrd with NQ <= K (N <= M case)
			if ( left ) {
				mi = M - 1;
				ni = N;
				i1 = 1;
				i2 = 0;
			} else {
				mi = M;
				ni = N - 1;
				i1 = 0;
				i2 = 1;
			}
			zunmlq(
				side, transt, mi, ni, nq - 1,
				A, sa1A, sa2A, offsetA + 2 * ( 0 * sa1A + 1 * sa2A ),
				TAU, strideTAU, offsetTAU,
				C, sc1, sc2, offsetC + 2 * ( i1 * sc1 + i2 * sc2 ),
				WORK, strideWORK, offsetWORK
			);
		}
	}
	return 0;
}


// MAIN //

/**
* Computes the singular value decomposition (SVD) of a complex M-by-N matrix A,
* optionally computing the left and/or right singular vectors.
*
* The SVD is written: A = U * SIGMA * conjugate-transpose(V)
*
* where SIGMA is an M-by-N matrix which is zero except for its min(M,N) diagonal
* elements, U is an M-by-M unitary matrix, and V is an N-by-N unitary matrix.
* The diagonal elements of SIGMA are the singular values of A; they are real and
* non-negative, and are returned in descending order. The first min(M,N) columns
* of U and V are the left and right singular vectors of A.
*
* @private
* @param {string} jobu - 'A': all M columns of U are returned, 'S': first min(M,N) columns, 'O': overwrite A, 'N': no U
* @param {string} jobvt - 'A': all N rows of V^H are returned, 'S': first min(M,N) rows, 'O': overwrite A, 'N': no VT
* @param {NonNegativeInteger} M - number of rows of A
* @param {NonNegativeInteger} N - number of columns of A
* @param {Float64Array} A - input/output matrix (interleaved complex, column-major)
* @param {integer} strideA1 - stride of the first dimension of A (in complex elements)
* @param {integer} strideA2 - stride of the second dimension of A (in complex elements)
* @param {NonNegativeInteger} offsetA - starting index for A (Float64 index)
* @param {Float64Array} s - output array of real singular values (length min(M,N))
* @param {integer} strideS - stride for s
* @param {NonNegativeInteger} offsetS - starting index for s
* @param {Float64Array} U - output matrix for left singular vectors (interleaved complex)
* @param {integer} strideU1 - stride of the first dimension of U
* @param {integer} strideU2 - stride of the second dimension of U
* @param {NonNegativeInteger} offsetU - starting index for U
* @param {Float64Array} VT - output matrix for right singular vectors (interleaved complex)
* @param {integer} strideVT1 - stride of the first dimension of VT
* @param {integer} strideVT2 - stride of the second dimension of VT
* @param {NonNegativeInteger} offsetVT - starting index for VT
* @param {Float64Array} WORK - complex workspace array (interleaved)
* @param {integer} strideWORK - stride for WORK
* @param {NonNegativeInteger} offsetWORK - starting index for WORK
* @param {integer} lwork - length of WORK array (in complex elements)
* @param {Float64Array} RWORK - real workspace array (length >= 5*min(M,N))
* @param {integer} strideRWORK - stride for RWORK
* @param {NonNegativeInteger} offsetRWORK - starting index for RWORK
* @returns {integer} info - 0 if successful, >0 if ZBDSQR did not converge
*/
function zgesvd( jobu, jobvt, M, N, A, strideA1, strideA2, offsetA, s, strideS, offsetS, U, strideU1, strideU2, offsetU, VT, strideVT1, strideVT2, offsetVT, WORK, strideWORK, offsetWORK, lwork, RWORK, strideRWORK, offsetRWORK ) { // eslint-disable-line max-len, max-params
	var wntua;
	var wntus;
	var wntuas;
	var wntuo;
	var wntun;
	var wntva;
	var wntvs;
	var wntvas;
	var wntvo;
	var wntvn;
	var minmn;
	var anrm;
	var bignum;
	var smlnum;
	var eps;
	var iscl;
	var info;
	var ierr;
	var ncu;
	var ncvt;
	var nru;
	var nrvt;
	var itau;
	var itauq;
	var itaup;
	var iwork;
	var irwork;
	var ie;
	var ir;
	var iu;
	var ldwrkr;
	var ldwrku;
	var chunk;
	var blk;
	var sa1;
	var sa2;
	var su1;
	var su2;
	var svt1;
	var svt2;
	var i;
	var WK;

	sa1 = strideA1;
	sa2 = strideA2;
	su1 = strideU1;
	su2 = strideU2;
	svt1 = strideVT1;
	svt2 = strideVT2;

	info = 0;
	minmn = Math.min( M, N );

	wntua = ( jobu === 'A' || jobu === 'a' );
	wntus = ( jobu === 'S' || jobu === 's' );
	wntuas = wntua || wntus;
	wntuo = ( jobu === 'O' || jobu === 'o' );
	wntun = ( jobu === 'N' || jobu === 'n' );
	wntva = ( jobvt === 'A' || jobvt === 'a' );
	wntvs = ( jobvt === 'S' || jobvt === 's' );
	wntvas = wntva || wntvs;
	wntvo = ( jobvt === 'O' || jobvt === 'o' );
	wntvn = ( jobvt === 'N' || jobvt === 'n' );

	// Quick return if possible
	if ( M === 0 || N === 0 ) {
		return 0;
	}

	// Allocate workspace: we use a flat Float64Array for complex workspace.
	// The work array in the Fortran code uses 1-based complex indices.
	// We allocate plenty of workspace internally.
	var wsz = Math.max( 1, 2 * ( 3 * minmn + Math.max( M, N ) + minmn * Math.max( M, N ) ) );
	WK = new Float64Array( wsz );

	// Compute machine parameters
	eps = dlamch( 'P' );
	smlnum = Math.sqrt( dlamch( 'S' ) ) / eps;
	bignum = 1.0 / smlnum;

	// Scale A if max element outside range [smlnum, bignum]
	var DUM = new Float64Array( Math.max( M, N ) );
	anrm = zlange( 'M', M, N, A, sa1, sa2, offsetA, DUM, 1, 0 );
	iscl = 0;
	if ( anrm > 0.0 && anrm < smlnum ) {
		iscl = 1;
		zlascl( 'G', 0, 0, anrm, smlnum, M, N, A, sa1, sa2, offsetA );
	} else if ( anrm > bignum ) {
		iscl = 1;
		zlascl( 'G', 0, 0, anrm, bignum, M, N, A, sa1, sa2, offsetA );
	}

	if ( M >= N ) {
		// A has at least as many rows as columns (M >= N)
		// Path: direct bidiagonal reduction (no initial QR factorization for simplicity)
		// This handles the case when M is not "much larger" than N.
		// For the case where M >> N, the optimal algorithm would do QR first,
		// but the direct path always works correctly.

		ie = 0;           // E in RWORK at offsetRWORK + ie
		itauq = 0;        // TAUQ in WK at 2*itauq
		itaup = itauq + N; // TAUP in WK at 2*itaup
		iwork = itaup + N; // WORK start in WK at 2*iwork

		// Bidiagonalize A (reduce to upper bidiagonal)
		zgebrd(
			M, N, A, sa1, sa2, offsetA,
			s, strideS, offsetS,
			RWORK, strideRWORK, offsetRWORK + ie,
			WK, 1, 2 * itauq,
			WK, 1, 2 * itaup,
			WK, 1, 2 * iwork,
			wsz - 2 * iwork
		);

		if ( wntuas ) {
			// Copy lower triangle of A to U, then generate Q
			zlacpy( 'L', M, N, A, 2 * sa1, 2 * sa2, offsetA, U, 2 * su1, 2 * su2, offsetU );
			ncu = wntus ? N : M;
			zungbr(
				'Q', M, ncu, N,
				U, su1, su2, offsetU,
				WK, 1, 2 * itauq,
				WK, 1, 2 * iwork
			);
		}
		if ( wntvas ) {
			// Copy upper triangle of A to VT, then generate P^H
			zlacpy( 'U', N, N, A, 2 * sa1, 2 * sa2, offsetA, VT, 2 * svt1, 2 * svt2, offsetVT );
			zungbr(
				'P', N, N, N,
				VT, svt1, svt2, offsetVT,
				WK, 1, 2 * itaup,
				WK, 1, 2 * iwork
			);
		}
		if ( wntuo ) {
			// Generate Q in A
			zungbr(
				'Q', M, N, N,
				A, sa1, sa2, offsetA,
				WK, 1, 2 * itauq,
				WK, 1, 2 * iwork
			);
		}
		if ( wntvo ) {
			// Generate P^H in A
			zungbr(
				'P', N, N, N,
				A, sa1, sa2, offsetA,
				WK, 1, 2 * itaup,
				WK, 1, 2 * iwork
			);
		}
		irwork = ie + N;

		// Determine dimensions for ZBDSQR
		nru = 0;
		ncvt = 0;
		if ( wntuas || wntuo ) {
			nru = M;
		}
		if ( wntvas || wntvo ) {
			ncvt = N;
		}

		// Perform bidiagonal SVD
		if ( ( !wntuo ) && ( !wntvo ) ) {
			// Neither U nor VT overwrite A
			info = zbdsqr(
				'U', N, ncvt, nru, 0,
				s, strideS, offsetS,
				RWORK, strideRWORK, offsetRWORK + ie,
				VT, svt1, svt2, offsetVT,
				U, su1, su2, offsetU,
				A, sa1, sa2, offsetA,  // C dummy (NCC=0)
				RWORK, strideRWORK, offsetRWORK + irwork
			);
		} else if ( ( !wntuo ) && wntvo ) {
			// VT overwrites A
			info = zbdsqr(
				'U', N, ncvt, nru, 0,
				s, strideS, offsetS,
				RWORK, strideRWORK, offsetRWORK + ie,
				A, sa1, sa2, offsetA,
				U, su1, su2, offsetU,
				A, sa1, sa2, offsetA, // C dummy (NCC=0)
				RWORK, strideRWORK, offsetRWORK + irwork
			);
		} else {
			// U overwrites A, or both overwrite A
			info = zbdsqr(
				'U', N, ncvt, nru, 0,
				s, strideS, offsetS,
				RWORK, strideRWORK, offsetRWORK + ie,
				VT, svt1, svt2, offsetVT,
				A, sa1, sa2, offsetA,
				A, sa1, sa2, offsetA, // C dummy (NCC=0)
				RWORK, strideRWORK, offsetRWORK + irwork
			);
		}
	} else {
		// M < N: A has more columns than rows
		// Direct bidiagonal reduction path

		ie = 0;           // E in RWORK
		itauq = 0;        // TAUQ in WK
		itaup = itauq + M; // TAUP in WK
		iwork = itaup + M; // WORK start in WK

		// Bidiagonalize A (reduce to lower bidiagonal when M < N)
		zgebrd(
			M, N, A, sa1, sa2, offsetA,
			s, strideS, offsetS,
			RWORK, strideRWORK, offsetRWORK + ie,
			WK, 1, 2 * itauq,
			WK, 1, 2 * itaup,
			WK, 1, 2 * iwork,
			wsz - 2 * iwork
		);

		if ( wntuas ) {
			// Copy lower triangle of A to U, then generate Q
			zlacpy( 'L', M, M, A, 2 * sa1, 2 * sa2, offsetA, U, 2 * su1, 2 * su2, offsetU );
			zungbr(
				'Q', M, M, N,
				U, su1, su2, offsetU,
				WK, 1, 2 * itauq,
				WK, 1, 2 * iwork
			);
		}
		if ( wntvas ) {
			// Copy upper triangle of A to VT, then generate P^H
			zlacpy( 'U', M, N, A, 2 * sa1, 2 * sa2, offsetA, VT, 2 * svt1, 2 * svt2, offsetVT );
			nrvt = wntva ? N : M;
			zungbr(
				'P', nrvt, N, M,
				VT, svt1, svt2, offsetVT,
				WK, 1, 2 * itaup,
				WK, 1, 2 * iwork
			);
		}
		if ( wntuo ) {
			// Generate Q in A
			zungbr(
				'Q', M, M, N,
				A, sa1, sa2, offsetA,
				WK, 1, 2 * itauq,
				WK, 1, 2 * iwork
			);
		}
		if ( wntvo ) {
			// Generate P^H in A
			zungbr(
				'P', M, N, M,
				A, sa1, sa2, offsetA,
				WK, 1, 2 * itaup,
				WK, 1, 2 * iwork
			);
		}
		irwork = ie + M;

		// Determine dimensions for ZBDSQR
		nru = 0;
		ncvt = 0;
		if ( wntuas || wntuo ) {
			nru = M;
		}
		if ( wntvas || wntvo ) {
			ncvt = N;
		}

		// Perform bidiagonal SVD (lower bidiagonal when M < N)
		if ( ( !wntuo ) && ( !wntvo ) ) {
			info = zbdsqr(
				'L', M, ncvt, nru, 0,
				s, strideS, offsetS,
				RWORK, strideRWORK, offsetRWORK + ie,
				VT, svt1, svt2, offsetVT,
				U, su1, su2, offsetU,
				A, sa1, sa2, offsetA,
				RWORK, strideRWORK, offsetRWORK + irwork
			);
		} else if ( ( !wntuo ) && wntvo ) {
			info = zbdsqr(
				'L', M, ncvt, nru, 0,
				s, strideS, offsetS,
				RWORK, strideRWORK, offsetRWORK + ie,
				A, sa1, sa2, offsetA,
				U, su1, su2, offsetU,
				A, sa1, sa2, offsetA,
				RWORK, strideRWORK, offsetRWORK + irwork
			);
		} else {
			info = zbdsqr(
				'L', M, ncvt, nru, 0,
				s, strideS, offsetS,
				RWORK, strideRWORK, offsetRWORK + ie,
				VT, svt1, svt2, offsetVT,
				A, sa1, sa2, offsetA,
				A, sa1, sa2, offsetA,
				RWORK, strideRWORK, offsetRWORK + irwork
			);
		}
	}

	// Undo scaling if necessary
	if ( iscl === 1 ) {
		if ( anrm > bignum ) {
			dlascl( 'G', 0, 0, bignum, anrm, minmn, 1, s, strideS, 1, offsetS );
		}
		if ( info !== 0 && anrm > bignum ) {
			dlascl( 'G', 0, 0, bignum, anrm, minmn - 1, 1, RWORK, strideRWORK, 1, offsetRWORK + ie );
		}
		if ( anrm < smlnum ) {
			dlascl( 'G', 0, 0, smlnum, anrm, minmn, 1, s, strideS, 1, offsetS );
		}
		if ( info !== 0 && anrm < smlnum ) {
			dlascl( 'G', 0, 0, smlnum, anrm, minmn - 1, 1, RWORK, strideRWORK, 1, offsetRWORK + ie );
		}
	}

	return info;
}


// EXPORTS //

module.exports = zgesvd;
