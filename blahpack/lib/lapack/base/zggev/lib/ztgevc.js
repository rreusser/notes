'use strict';

// MODULES //

var dlamch = require( '../../../../lapack/base/dlamch/lib/base.js' );
var zladiv = require( '../../../../lapack/base/zladiv/lib/base.js' );
var zgemv = require( '../../../../blas/base/zgemv/lib/base.js' );

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
* Compute some or all right and/or left eigenvectors of a pair of
* complex upper triangular matrices (S, P).
*
* Matrix pair (S, P) must be in Schur form: S upper triangular (generalized
* Hessenberg reduction output), P upper triangular with real positive diagonal.
*
* Complex elements are stored as interleaved real/imaginary pairs.
* Matrix strides are in doubles.
*
* @private
* @param {string} side - 'R' for right, 'L' for left, 'B' for both
* @param {string} howmny - 'A' for all, 'B' for backtransform, 'S' for selected
* @param {NonNegativeInteger} N - order of matrices
* @param {Float64Array} S - upper triangular matrix (Schur form of A)
* @param {integer} strideS1 - first dim stride of S (doubles)
* @param {integer} strideS2 - second dim stride of S (doubles)
* @param {NonNegativeInteger} offsetS - starting index for S
* @param {Float64Array} P - upper triangular matrix (Schur form of B)
* @param {integer} strideP1 - first dim stride of P (doubles)
* @param {integer} strideP2 - second dim stride of P (doubles)
* @param {NonNegativeInteger} offsetP - starting index for P
* @param {Float64Array} VL - left eigenvectors (interleaved complex, modified in-place)
* @param {integer} strideVL1 - first dim stride of VL (doubles)
* @param {integer} strideVL2 - second dim stride of VL (doubles)
* @param {NonNegativeInteger} offsetVL - starting index for VL
* @param {Float64Array} VR - right eigenvectors (interleaved complex, modified in-place)
* @param {integer} strideVR1 - first dim stride of VR (doubles)
* @param {integer} strideVR2 - second dim stride of VR (doubles)
* @param {NonNegativeInteger} offsetVR - starting index for VR
* @param {NonNegativeInteger} MM - number of columns in VL/VR
* @param {Float64Array} WORK - workspace (interleaved complex, length >= 2*N)
* @param {Float64Array} RWORK - real workspace (length >= 2*N)
* @returns {integer} 0 on success
*/
function ztgevc( side, howmny, N, S, strideS1, strideS2, offsetS, P, strideP1, strideP2, offsetP, VL, strideVL1, strideVL2, offsetVL, VR, strideVR1, strideVR2, offsetVR, MM, WORK, RWORK ) { // eslint-disable-line max-len, max-params
	var bignum;
	var safmin;
	var acoefa;
	var acoeff;
	var ascale;
	var bcoefa;
	var bscale;
	var anorm;
	var bnorm;
	var compl;
	var compr;
	var small;
	var scale;
	var ilback;
	var salpha;
	var sbeta;
	var dmin;
	var temp;
	var xmax;
	var big;
	var ulp;
	var lsa;
	var lsb;
	var sS1;
	var sS2;
	var sP1;
	var sP2;
	var oS;
	var oP;
	var sVL1;
	var sVL2;
	var oVL;
	var sVR1;
	var sVR2;
	var oVR;
	var bcoeff;
	var negSum;
	var negW;
	var divOut;
	var ieig;
	var isrc;
	var ibeg;
	var iend;
	var sum;
	var d;
	var je;
	var jr;
	var ca;
	var cb;
	var j;
	var i;
	var idx;
	var idx2;

	compl = ( side === 'L' || side === 'l' || side === 'B' || side === 'b' );
	compr = ( side === 'R' || side === 'r' || side === 'B' || side === 'b' );
	ilback = ( howmny === 'B' || howmny === 'b' );

	if ( N === 0 ) {
		return 0;
	}

	sS1 = strideS1;
	sS2 = strideS2;
	oS = offsetS;
	sP1 = strideP1;
	sP2 = strideP2;
	oP = offsetP;
	sVL1 = strideVL1;
	sVL2 = strideVL2;
	oVL = offsetVL;
	sVR1 = strideVR1;
	sVR2 = strideVR2;
	oVR = offsetVR;

	// Machine constants
	safmin = dlamch( 'Safe minimum' );
	big = ONE / safmin;
	ulp = dlamch( 'Epsilon' ) * dlamch( 'Base' );
	small = safmin * N / ulp;
	big = ONE / small;
	bignum = ONE / ( safmin * N );

	// Compute norms of S and P
	anorm = abs1( S, oS );
	bnorm = abs1( P, oP );
	RWORK[ 0 ] = ZERO;
	RWORK[ N ] = ZERO;
	for ( j = 1; j < N; j++ ) {
		RWORK[ j ] = ZERO;
		RWORK[ N + j ] = ZERO;
		for ( i = 0; i < j; i++ ) {
			RWORK[ j ] += abs1( S, oS + i * sS1 + j * sS2 );
			RWORK[ N + j ] += abs1( P, oP + i * sP1 + j * sP2 );
		}
		anorm = Math.max( anorm, RWORK[ j ] + abs1( S, oS + j * sS1 + j * sS2 ) );
		bnorm = Math.max( bnorm, RWORK[ N + j ] + abs1( P, oP + j * sP1 + j * sP2 ) );
	}

	ascale = ONE / Math.max( anorm, safmin );
	bscale = ONE / Math.max( bnorm, safmin );

	// Temporary complex scalars
	salpha = new Float64Array( 2 );
	bcoeff = new Float64Array( 2 );
	sum = new Float64Array( 2 );
	d = new Float64Array( 2 );
	ca = new Float64Array( 2 );
	cb = new Float64Array( 2 );
	negSum = new Float64Array( 2 );
	negW = new Float64Array( 2 );
	divOut = new Float64Array( 2 );

	// ========================
	// LEFT EIGENVECTORS
	// ========================
	if ( compl ) {
		ieig = 0;

		for ( je = 0; je < N; je++ ) {
			ieig++;

			idx = oS + je * sS1 + je * sS2;
			idx2 = oP + je * sP1 + je * sP2;

			if ( abs1( S, idx ) <= safmin && Math.abs( P[ idx2 ] ) <= safmin ) {
				// Zero eigenvalue - set eigenvector to unit vector
				for ( jr = 0; jr < N; jr++ ) {
					VL[ oVL + jr * sVL1 + ( ieig - 1 ) * sVL2 ] = 0.0;
					VL[ oVL + jr * sVL1 + ( ieig - 1 ) * sVL2 + 1 ] = 0.0;
				}
				VL[ oVL + ( ieig - 1 ) * sVL1 + ( ieig - 1 ) * sVL2 ] = 1.0;
				VL[ oVL + ( ieig - 1 ) * sVL1 + ( ieig - 1 ) * sVL2 + 1 ] = 0.0;
				continue;
			}

			// Compute scaling
			temp = ONE / Math.max(
				abs1( S, idx ) * ascale,
				Math.abs( P[ idx2 ] ) * bscale,
				safmin
			);
			salpha[ 0 ] = ( temp * S[ idx ] ) * ascale;
			salpha[ 1 ] = ( temp * S[ idx + 1 ] ) * ascale;
			sbeta = ( temp * P[ idx2 ] ) * bscale;
			acoeff = sbeta * ascale;
			bcoeff[ 0 ] = salpha[ 0 ] * bscale;
			bcoeff[ 1 ] = salpha[ 1 ] * bscale;

			// Scale to avoid overflow
			lsa = Math.abs( sbeta ) >= safmin && Math.abs( acoeff ) < small;
			lsb = abs1( salpha, 0 ) >= safmin && abs1( bcoeff, 0 ) < small;

			scale = ONE;
			if ( lsa ) {
				scale = ( small / Math.abs( sbeta ) ) * Math.min( anorm, big );
			}
			if ( lsb ) {
				scale = Math.max( scale, ( small / abs1( salpha, 0 ) ) * Math.min( bnorm, big ) );
			}
			if ( lsa || lsb ) {
				scale = Math.min( scale, ONE / ( safmin * Math.max( ONE, Math.abs( acoeff ), abs1( bcoeff, 0 ) ) ) );
				if ( lsa ) {
					acoeff = ascale * ( scale * sbeta );
				} else {
					acoeff = scale * acoeff;
				}
				if ( lsb ) {
					bcoeff[ 0 ] = bscale * ( scale * salpha[ 0 ] );
					bcoeff[ 1 ] = bscale * ( scale * salpha[ 1 ] );
				} else {
					bcoeff[ 0 ] = scale * bcoeff[ 0 ];
					bcoeff[ 1 ] = scale * bcoeff[ 1 ];
				}
			}

			acoefa = Math.abs( acoeff );
			bcoefa = abs1( bcoeff, 0 );
			xmax = ONE;

			// Initialize WORK
			for ( jr = 0; jr < N; jr++ ) {
				WORK[ jr * 2 ] = 0.0;
				WORK[ jr * 2 + 1 ] = 0.0;
			}
			WORK[ je * 2 ] = 1.0;
			WORK[ je * 2 + 1 ] = 0.0;

			dmin = Math.max( ulp * acoefa * anorm, ulp * bcoefa * bnorm, safmin );

			// Triangular solve: forward substitution for left eigenvectors
			for ( j = je + 1; j < N; j++ ) {
				// Scale to avoid overflow
				temp = ONE / xmax;
				if ( acoefa * RWORK[ j ] + bcoefa * RWORK[ N + j ] > bignum * temp ) {
					for ( jr = je; jr < j; jr++ ) {
						WORK[ jr * 2 ] *= temp;
						WORK[ jr * 2 + 1 ] *= temp;
					}
					xmax = ONE;
				}

				// Compute sum
				var sumaRe = 0.0; // eslint-disable-line no-var
				var sumaIm = 0.0; // eslint-disable-line no-var
				var sumbRe = 0.0; // eslint-disable-line no-var
				var sumbIm = 0.0; // eslint-disable-line no-var

				for ( jr = je; jr < j; jr++ ) {
					// SUMA += CONJG(S(JR,J)) * WORK(JR)
					var sr = S[ oS + jr * sS1 + j * sS2 ]; // eslint-disable-line no-var
					var si = -S[ oS + jr * sS1 + j * sS2 + 1 ]; // eslint-disable-line no-var, max-len
					sumaRe += sr * WORK[ jr * 2 ] - si * WORK[ jr * 2 + 1 ];
					sumaIm += sr * WORK[ jr * 2 + 1 ] + si * WORK[ jr * 2 ];

					// SUMB += CONJG(P(JR,J)) * WORK(JR)
					var pr = P[ oP + jr * sP1 + j * sP2 ]; // eslint-disable-line no-var
					var pi = -P[ oP + jr * sP1 + j * sP2 + 1 ]; // eslint-disable-line no-var, max-len
					sumbRe += pr * WORK[ jr * 2 ] - pi * WORK[ jr * 2 + 1 ];
					sumbIm += pr * WORK[ jr * 2 + 1 ] + pi * WORK[ jr * 2 ];
				}

				// SUM = ACOEFF * SUMA - CONJG(BCOEFF) * SUMB
				sum[ 0 ] = acoeff * sumaRe - ( bcoeff[ 0 ] * sumbRe + bcoeff[ 1 ] * sumbIm );
				sum[ 1 ] = acoeff * sumaIm - ( bcoeff[ 0 ] * sumbIm - bcoeff[ 1 ] * sumbRe );

				// D = CONJG(ACOEFF * S(J,J) - BCOEFF * P(J,J))
				var sjjr = S[ oS + j * sS1 + j * sS2 ]; // eslint-disable-line no-var
				var sjji = S[ oS + j * sS1 + j * sS2 + 1 ]; // eslint-disable-line no-var
				var pjjr = P[ oP + j * sP1 + j * sP2 ]; // eslint-disable-line no-var
				var pjji = P[ oP + j * sP1 + j * sP2 + 1 ]; // eslint-disable-line no-var

				// acoeff * S(j,j) - bcoeff * P(j,j), then conjugate
				var dr = acoeff * sjjr - ( bcoeff[ 0 ] * pjjr - bcoeff[ 1 ] * pjji ); // eslint-disable-line no-var, max-len
				var di = acoeff * sjji - ( bcoeff[ 0 ] * pjji + bcoeff[ 1 ] * pjjr ); // eslint-disable-line no-var, max-len
				d[ 0 ] = dr;
				d[ 1 ] = -di; // conjugate

				if ( abs1( d, 0 ) <= dmin ) {
					d[ 0 ] = dmin;
					d[ 1 ] = 0.0;
				}

				if ( abs1( d, 0 ) < ONE ) {
					if ( abs1( sum, 0 ) >= bignum * abs1( d, 0 ) ) {
						temp = ONE / abs1( sum, 0 );
						for ( jr = je; jr < j; jr++ ) {
							WORK[ jr * 2 ] *= temp;
							WORK[ jr * 2 + 1 ] *= temp;
						}
						xmax *= temp;
						sum[ 0 ] *= temp;
						sum[ 1 ] *= temp;
					}
				}

				// WORK(J) = ZLADIV(-SUM, D)
				negSum[ 0 ] = -sum[ 0 ];
				negSum[ 1 ] = -sum[ 1 ];
				zladiv( negSum, d, divOut );
				WORK[ j * 2 ] = divOut[ 0 ];
				WORK[ j * 2 + 1 ] = divOut[ 1 ];
				xmax = Math.max( xmax, abs1( WORK, j * 2 ) );
			}

			// Back-transform or copy result
			if ( ilback ) {
				// WORK(N+1:2N) = VL * WORK(JE:N)
				zgemv(
					'N', N, N - je, CONE, VL, sVL1 / 2, sVL2 / 2, oVL + je * sVL2,
					WORK, 1, je * 2, CZERO, WORK, 1, N * 2
				);
				isrc = 1; // result in WORK[N*2..]
				ibeg = 0;
			} else {
				isrc = 0; // result in WORK[0..]
				ibeg = je;
			}

			// Normalize and store in VL
			xmax = ZERO;
			for ( jr = ibeg; jr < N; jr++ ) {
				xmax = Math.max( xmax, abs1( WORK, isrc * N * 2 + jr * 2 ) );
			}

			if ( xmax > safmin ) {
				temp = ONE / xmax;
				for ( jr = ibeg; jr < N; jr++ ) {
					VL[ oVL + jr * sVL1 + ( ieig - 1 ) * sVL2 ] = temp * WORK[ isrc * N * 2 + jr * 2 ];
					VL[ oVL + jr * sVL1 + ( ieig - 1 ) * sVL2 + 1 ] = temp * WORK[ isrc * N * 2 + jr * 2 + 1 ];
				}
			} else {
				ibeg = N;
			}

			for ( jr = 0; jr < ibeg; jr++ ) {
				VL[ oVL + jr * sVL1 + ( ieig - 1 ) * sVL2 ] = 0.0;
				VL[ oVL + jr * sVL1 + ( ieig - 1 ) * sVL2 + 1 ] = 0.0;
			}
		}
	}

	// ========================
	// RIGHT EIGENVECTORS
	// ========================
	if ( compr ) {
		ieig = N;

		for ( je = N - 1; je >= 0; je-- ) {
			idx = oS + je * sS1 + je * sS2;
			idx2 = oP + je * sP1 + je * sP2;

			if ( abs1( S, idx ) <= safmin && Math.abs( P[ idx2 ] ) <= safmin ) {
				// Zero eigenvalue
				for ( jr = 0; jr < N; jr++ ) {
					VR[ oVR + jr * sVR1 + ( ieig - 1 ) * sVR2 ] = 0.0;
					VR[ oVR + jr * sVR1 + ( ieig - 1 ) * sVR2 + 1 ] = 0.0;
				}
				VR[ oVR + ( ieig - 1 ) * sVR1 + ( ieig - 1 ) * sVR2 ] = 1.0;
				VR[ oVR + ( ieig - 1 ) * sVR1 + ( ieig - 1 ) * sVR2 + 1 ] = 0.0;
				ieig--;
				continue;
			}

			// Compute scaling
			temp = ONE / Math.max(
				abs1( S, idx ) * ascale,
				Math.abs( P[ idx2 ] ) * bscale,
				safmin
			);
			salpha[ 0 ] = ( temp * S[ idx ] ) * ascale;
			salpha[ 1 ] = ( temp * S[ idx + 1 ] ) * ascale;
			sbeta = ( temp * P[ idx2 ] ) * bscale;
			acoeff = sbeta * ascale;
			bcoeff[ 0 ] = salpha[ 0 ] * bscale;
			bcoeff[ 1 ] = salpha[ 1 ] * bscale;

			lsa = Math.abs( sbeta ) >= safmin && Math.abs( acoeff ) < small;
			lsb = abs1( salpha, 0 ) >= safmin && abs1( bcoeff, 0 ) < small;

			scale = ONE;
			if ( lsa ) {
				scale = ( small / Math.abs( sbeta ) ) * Math.min( anorm, big );
			}
			if ( lsb ) {
				scale = Math.max( scale, ( small / abs1( salpha, 0 ) ) * Math.min( bnorm, big ) );
			}
			if ( lsa || lsb ) {
				scale = Math.min( scale, ONE / ( safmin * Math.max( ONE, Math.abs( acoeff ), abs1( bcoeff, 0 ) ) ) );
				if ( lsa ) {
					acoeff = ascale * ( scale * sbeta );
				} else {
					acoeff = scale * acoeff;
				}
				if ( lsb ) {
					bcoeff[ 0 ] = bscale * ( scale * salpha[ 0 ] );
					bcoeff[ 1 ] = bscale * ( scale * salpha[ 1 ] );
				} else {
					bcoeff[ 0 ] = scale * bcoeff[ 0 ];
					bcoeff[ 1 ] = scale * bcoeff[ 1 ];
				}
			}

			acoefa = Math.abs( acoeff );
			bcoefa = abs1( bcoeff, 0 );
			xmax = ONE;

			// Initialize WORK
			for ( jr = 0; jr < N; jr++ ) {
				WORK[ jr * 2 ] = 0.0;
				WORK[ jr * 2 + 1 ] = 0.0;
			}
			WORK[ je * 2 ] = 1.0;
			WORK[ je * 2 + 1 ] = 0.0;

			dmin = Math.max( ulp * acoefa * anorm, ulp * bcoefa * bnorm, safmin );

			// Initialize WORK(1:JE-1) = ACOEFF * S(:,JE) - BCOEFF * P(:,JE)
			for ( jr = 0; jr < je; jr++ ) {
				var srr = S[ oS + jr * sS1 + je * sS2 ]; // eslint-disable-line no-var
				var sri = S[ oS + jr * sS1 + je * sS2 + 1 ]; // eslint-disable-line no-var
				var prr = P[ oP + jr * sP1 + je * sP2 ]; // eslint-disable-line no-var
				var pri = P[ oP + jr * sP1 + je * sP2 + 1 ]; // eslint-disable-line no-var
				WORK[ jr * 2 ] = acoeff * srr - ( bcoeff[ 0 ] * prr - bcoeff[ 1 ] * pri );
				WORK[ jr * 2 + 1 ] = acoeff * sri - ( bcoeff[ 0 ] * pri + bcoeff[ 1 ] * prr );
			}

			// Back-substitution
			for ( j = je - 1; j >= 0; j-- ) {
				// D = ACOEFF * S(J,J) - BCOEFF * P(J,J)
				sjjr = S[ oS + j * sS1 + j * sS2 ]; // eslint-disable-line no-redeclare
				sjji = S[ oS + j * sS1 + j * sS2 + 1 ]; // eslint-disable-line no-redeclare
				pjjr = P[ oP + j * sP1 + j * sP2 ]; // eslint-disable-line no-redeclare
				pjji = P[ oP + j * sP1 + j * sP2 + 1 ]; // eslint-disable-line no-redeclare

				d[ 0 ] = acoeff * sjjr - ( bcoeff[ 0 ] * pjjr - bcoeff[ 1 ] * pjji );
				d[ 1 ] = acoeff * sjji - ( bcoeff[ 0 ] * pjji + bcoeff[ 1 ] * pjjr );

				if ( abs1( d, 0 ) <= dmin ) {
					d[ 0 ] = dmin;
					d[ 1 ] = 0.0;
				}

				if ( abs1( d, 0 ) < ONE ) {
					if ( abs1( WORK, j * 2 ) >= bignum * abs1( d, 0 ) ) {
						temp = ONE / abs1( WORK, j * 2 );
						for ( jr = 0; jr <= je; jr++ ) {
							WORK[ jr * 2 ] *= temp;
							WORK[ jr * 2 + 1 ] *= temp;
						}
					}
				}

				// WORK(J) = ZLADIV(-WORK(J), D)
				negW[ 0 ] = -WORK[ j * 2 ];
				negW[ 1 ] = -WORK[ j * 2 + 1 ];
				zladiv( negW, d, divOut );
				WORK[ j * 2 ] = divOut[ 0 ];
				WORK[ j * 2 + 1 ] = divOut[ 1 ];

				if ( j > 0 ) {
					// Scale to avoid overflow and update
					if ( abs1( WORK, j * 2 ) > ONE ) {
						temp = ONE / abs1( WORK, j * 2 );
						if ( acoefa * RWORK[ j ] + bcoefa * RWORK[ N + j ] >= bignum * temp ) {
							for ( jr = 0; jr <= je; jr++ ) {
								WORK[ jr * 2 ] *= temp;
								WORK[ jr * 2 + 1 ] *= temp;
							}
						}
					}

					// CA = ACOEFF * WORK(J)
					ca[ 0 ] = acoeff * WORK[ j * 2 ];
					ca[ 1 ] = acoeff * WORK[ j * 2 + 1 ];
					// CB = BCOEFF * WORK(J)
					cb[ 0 ] = bcoeff[ 0 ] * WORK[ j * 2 ] - bcoeff[ 1 ] * WORK[ j * 2 + 1 ];
					cb[ 1 ] = bcoeff[ 0 ] * WORK[ j * 2 + 1 ] + bcoeff[ 1 ] * WORK[ j * 2 ];

					for ( jr = 0; jr < j; jr++ ) {
						// WORK(JR) += CA * S(JR,J) - CB * P(JR,J)
						sr = S[ oS + jr * sS1 + j * sS2 ]; // eslint-disable-line no-redeclare
						si = S[ oS + jr * sS1 + j * sS2 + 1 ]; // eslint-disable-line no-redeclare
						pr = P[ oP + jr * sP1 + j * sP2 ]; // eslint-disable-line no-redeclare
						pi = P[ oP + jr * sP1 + j * sP2 + 1 ]; // eslint-disable-line no-redeclare

						WORK[ jr * 2 ] += ( ca[ 0 ] * sr - ca[ 1 ] * si ) - ( cb[ 0 ] * pr - cb[ 1 ] * pi );
						WORK[ jr * 2 + 1 ] += ( ca[ 0 ] * si + ca[ 1 ] * sr ) - ( cb[ 0 ] * pi + cb[ 1 ] * pr );
					}
				}
			}

			// Back-transform or copy result
			if ( ilback ) {
				zgemv(
					'N', N, je + 1, CONE, VR, sVR1 / 2, sVR2 / 2, oVR,
					WORK, 1, 0, CZERO, WORK, 1, N * 2
				);
				isrc = 1;
				iend = N;
			} else {
				isrc = 0;
				iend = je + 1;
			}

			// Normalize and store in VR
			xmax = ZERO;
			for ( jr = 0; jr < iend; jr++ ) {
				xmax = Math.max( xmax, abs1( WORK, isrc * N * 2 + jr * 2 ) );
			}

			if ( xmax > safmin ) {
				temp = ONE / xmax;
				for ( jr = 0; jr < iend; jr++ ) {
					VR[ oVR + jr * sVR1 + ( ieig - 1 ) * sVR2 ] = temp * WORK[ isrc * N * 2 + jr * 2 ];
					VR[ oVR + jr * sVR1 + ( ieig - 1 ) * sVR2 + 1 ] = temp * WORK[ isrc * N * 2 + jr * 2 + 1 ];
				}
			} else {
				iend = 0;
			}

			for ( jr = iend; jr < N; jr++ ) {
				VR[ oVR + jr * sVR1 + ( ieig - 1 ) * sVR2 ] = 0.0;
				VR[ oVR + jr * sVR1 + ( ieig - 1 ) * sVR2 + 1 ] = 0.0;
			}

			ieig--;
		}
	}

	return 0;
}


// EXPORTS //

module.exports = ztgevc;
