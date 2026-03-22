/**
* @license Apache-2.0
*
* Copyright (c) 2025 The Stdlib Authors.
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
var dlamch = require( '../../dlamch/lib/base.js' );
var zlanhs = require( '../../zlanhs/lib/base.js' );
var zlaset = require( '../../zlaset/lib/base.js' );
var zlartg = require( '../../zlartg/lib/base.js' );
var zladiv = require( '../../zladiv/lib/base.js' );
var zscal = require( '../../../../blas/base/zscal/lib/base.js' );
var zrot = require( '../../zrot/lib/base.js' );
var cmplx = require( '../../../../cmplx.js' );

// VARIABLES //

var ZERO = 0.0;
var ONE = 1.0;
var HALF = 0.5;

// MAIN //

/**
* Compute the eigenvalues of a complex matrix pair (H, T), where H is
* upper Hessenberg and T is upper triangular, using the single-shift QZ
* method. Optionally compute the Schur form and/or Schur vectors.
*
* Complex elements are stored as interleaved real/imaginary pairs.
* Element (i, j) has real part at `offset + i*stride1 + j*stride2` and
* imaginary part at `offset + i*stride1 + j*stride2 + 1`.
*
* @private
* @param {string} job - 'E': eigenvalues only; 'S': Schur form
* @param {string} compq - 'N': no Q; 'I': initialize Q to I; 'V': accumulate into Q
* @param {string} compz - 'N': no Z; 'I': initialize Z to I; 'V': accumulate into Z
* @param {NonNegativeInteger} N - order of matrices H, T, Q, Z
* @param {integer} ilo - start of active block (1-based)
* @param {integer} ihi - end of active block (1-based)
* @param {Complex128Array} H - upper Hessenberg matrix
* @param {integer} strideH1 - row stride for H (complex elements)
* @param {integer} strideH2 - column stride for H (complex elements)
* @param {NonNegativeInteger} offsetH - starting index for H (complex elements)
* @param {Complex128Array} T - upper triangular matrix
* @param {integer} strideT1 - row stride for T (complex elements)
* @param {integer} strideT2 - column stride for T (complex elements)
* @param {NonNegativeInteger} offsetT - starting index for T (complex elements)
* @param {Complex128Array} ALPHA - output eigenvalue numerators
* @param {integer} strideALPHA - stride for ALPHA (complex elements)
* @param {NonNegativeInteger} offsetALPHA - starting index for ALPHA (complex elements)
* @param {Complex128Array} BETA - output eigenvalue denominators
* @param {integer} strideBETA - stride for BETA (complex elements)
* @param {NonNegativeInteger} offsetBETA - starting index for BETA (complex elements)
* @param {Complex128Array} Q - left Schur vectors
* @param {integer} strideQ1 - row stride for Q (complex elements)
* @param {integer} strideQ2 - column stride for Q (complex elements)
* @param {NonNegativeInteger} offsetQ - starting index for Q (complex elements)
* @param {Complex128Array} Z - right Schur vectors
* @param {integer} strideZ1 - row stride for Z (complex elements)
* @param {integer} strideZ2 - column stride for Z (complex elements)
* @param {NonNegativeInteger} offsetZ - starting index for Z (complex elements)
* @param {Complex128Array} WORK - workspace
* @param {integer} strideWORK - stride for WORK (complex elements)
* @param {NonNegativeInteger} offsetWORK - starting index for WORK (complex elements)
* @param {integer} lwork - workspace size (in complex elements); -1 for query
* @param {Float64Array} RWORK - real workspace (length >= N)
* @param {integer} strideRWORK - stride for RWORK
* @param {NonNegativeInteger} offsetRWORK - starting index for RWORK
* @returns {integer} INFO: 0=success, 1..N=QZ did not converge, N+1..2N=shift failed
*/
function zhgeqz( job, compq, compz, N, ilo, ihi, H, strideH1, strideH2, offsetH, T, strideT1, strideT2, offsetT, ALPHA, strideALPHA, offsetALPHA, BETA, strideBETA, offsetBETA, Q, strideQ1, strideQ2, offsetQ, Z, strideZ1, strideZ2, offsetZ, WORK, strideWORK, offsetWORK, lwork, RWORK, strideRWORK, offsetRWORK ) { // eslint-disable-line max-len, max-params
	var ilschr;
	var ilazr2;
	var ilazro;
	var ifirst;
	var ifrstm;
	var ilastm;
	var istart;
	var signbc;
	var ctemp2;
	var ctemp3;
	var eshift;
	var ascale;
	var bscale;
	var safmin;
	var shift;
	var abi22;
	var abi12;
	var anorm;
	var bnorm;
	var ctemp;
	var iiter;
	var ilast;
	var maxit;
	var jiter;
	var done;
	var tempr;
	var temp2;
	var absb;
	var atol;
	var btol;
	var info;
	var temp;
	var ad11;
	var ad12;
	var ad21;
	var ad22;
	var u12;
	var ilq;
	var ilz;
	var ulp;
	var sh1;
	var sh2;
	var sq1;
	var sq2;
	var st1;
	var st2;
	var sz1;
	var sz2;
	var rq1;
	var rz1;
	var in0;
	var jc;
	var jr;
	var jch;
	var out;
	var s;
	var c;
	var j;
	var f;
	var g;
	var x;
	var y;

	// Decode JOB
	if ( job === 'E' || job === 'e' ) {
		ilschr = false;
	} else if ( job === 'S' || job === 's' ) {
		ilschr = true;
	} else {
		return -1;
	}

	// Decode COMPQ
	if ( compq === 'N' || compq === 'n' ) {
		ilq = false;
	} else if ( compq === 'V' || compq === 'v' ) {
		ilq = true;
	} else if ( compq === 'I' || compq === 'i' ) {
		ilq = true;
	} else {
		return -2;
	}

	// Decode COMPZ
	if ( compz === 'N' || compz === 'n' ) {
		ilz = false;
	} else if ( compz === 'V' || compz === 'v' ) {
		ilz = true;
	} else if ( compz === 'I' || compz === 'i' ) {
		ilz = true;
	} else {
		return -3;
	}

	info = 0;

	// Quick return if N <= 0
	if ( N <= 0 ) {
		return 0;
	}

	// Get Float64Array views for direct element access
	var Hv = reinterpret( H, 0 ); // eslint-disable-line no-var
	var Tv = reinterpret( T, 0 ); // eslint-disable-line no-var
	var ALPHAv = reinterpret( ALPHA, 0 ); // eslint-disable-line no-var
	var BETAv = reinterpret( BETA, 0 ); // eslint-disable-line no-var
	var Qv = reinterpret( Q, 0 ); // eslint-disable-line no-var
	var Zv = reinterpret( Z, 0 ); // eslint-disable-line no-var

	// Float64 strides and offsets (for direct element access)
	sh1 = strideH1 * 2;
	sh2 = strideH2 * 2;
	st1 = strideT1 * 2;
	st2 = strideT2 * 2;
	sq1 = strideQ1 * 2;
	sq2 = strideQ2 * 2;
	sz1 = strideZ1 * 2;
	sz2 = strideZ2 * 2;

	// Float64 base offsets for element access
	var oH = offsetH * 2; // eslint-disable-line no-var
	var oT = offsetT * 2; // eslint-disable-line no-var
	var oAL = offsetALPHA * 2; // eslint-disable-line no-var
	var oBE = offsetBETA * 2; // eslint-disable-line no-var
	var sAL = strideALPHA * 2; // eslint-disable-line no-var
	var sBE = strideBETA * 2; // eslint-disable-line no-var

	// zrot strides: in complex elements (same as input strides now)
	rq1 = strideQ1;
	rz1 = strideZ1;

	// Working arrays for zlartg
	f = new Float64Array( 2 );
	g = new Float64Array( 2 );
	out = new Float64Array( 5 ); // [c, s_re, s_im, r_re, r_im]
	s = new Float64Array( 2 );   // complex s for zrot
	signbc = new Float64Array( 2 );
	ctemp = new Float64Array( 2 );
	ctemp2 = new Float64Array( 2 );
	ctemp3 = new Float64Array( 2 );
	eshift = new Float64Array( 2 ); // (0, 0)
	shift = new Float64Array( 2 );
	ad11 = new Float64Array( 2 );
	ad12 = new Float64Array( 2 );
	ad21 = new Float64Array( 2 );
	ad22 = new Float64Array( 2 );
	u12 = new Float64Array( 2 );
	abi22 = new Float64Array( 2 );
	abi12 = new Float64Array( 2 );
	x = new Float64Array( 2 );
	y = new Float64Array( 2 );

	// Initialize Q and Z to identity if requested
	if ( compq === 'I' || compq === 'i' ) {
		zlaset( 'Full', N, N,
			new Float64Array( [ 0.0, 0.0 ] ),
			new Float64Array( [ 1.0, 0.0 ] ),
			Q, sq1, sq2, offsetQ
		);
	}
	if ( compz === 'I' || compz === 'i' ) {
		zlaset( 'Full', N, N,
			new Float64Array( [ 0.0, 0.0 ] ),
			new Float64Array( [ 1.0, 0.0 ] ),
			Z, sz1, sz2, offsetZ
		);
	}

	// Machine constants
	in0 = ihi + 1 - ilo;
	safmin = dlamch( 'S' );
	ulp = dlamch( 'E' ) * dlamch( 'B' );

	// Compute norms of active submatrix
	// ZLANHS('F', IN, H(ILO,ILO), LDH, RWORK)
	// 0-based: H at (ilo-1, ilo-1)
	// zlanhs expects strides in complex elements (each = 2 doubles)
	anorm = zlanhs( 'F', in0, H, strideH1, strideH2, offsetH + ( ilo - 1 ) * strideH1 + ( ilo - 1 ) * strideH2, RWORK, strideRWORK, offsetRWORK );
	bnorm = zlanhs( 'F', in0, T, strideT1, strideT2, offsetT + ( ilo - 1 ) * strideT1 + ( ilo - 1 ) * strideT2, RWORK, strideRWORK, offsetRWORK );
	atol = Math.max( safmin, ulp * anorm );
	btol = Math.max( safmin, ulp * bnorm );
	ascale = ONE / Math.max( safmin, anorm );
	bscale = ONE / Math.max( safmin, bnorm );

	// Set Eigenvalues IHI+1:N (0-based: indices ihi to N-1)
	for ( j = ihi; j < N; j++ ) {
		absb = cabs( Tv, oT + j * st1 + j * st2 );
		if ( absb > safmin ) {
			// signbc = conj(T(j,j) / absb)
			cscaleConj( signbc, Tv, oT + j * st1 + j * st2, absb );
			// T(j,j) = absb
			Tv[ oT + j * st1 + j * st2 ] = absb;
			Tv[ oT + j * st1 + j * st2 + 1 ] = ZERO;
			if ( ilschr ) {
				// ZSCAL(j, signbc, T(1,j), 1) -- j elements (0-based: j elements starting from row 0)
				zscal( j, signbc, T, strideT1, offsetT + j * strideT2 );
				// ZSCAL(j+1, signbc, H(1,j), 1) -- j+1 elements
				zscal( j + 1, signbc, H, strideH1, offsetH + j * strideH2 );
			} else {
				// ZSCAL(1, signbc, H(j,j), 1)
				zscal( 1, signbc, H, 1, offsetH + j * strideH1 + j * strideH2 );
			}
			if ( ilz ) {
				zscal( N, signbc, Z, strideZ1, offsetZ + j * strideZ2 );
			}
		} else {
			Tv[ oT + j * st1 + j * st2 ] = ZERO;
			Tv[ oT + j * st1 + j * st2 + 1 ] = ZERO;
		}
		// ALPHA(j) = H(j,j), BETA(j) = T(j,j)
		ALPHAv[ oAL + j * sAL ] = Hv[ oH + j * sh1 + j * sh2 ];
		ALPHAv[ oAL + j * sAL + 1 ] = Hv[ oH + j * sh1 + j * sh2 + 1 ];
		BETAv[ oBE + j * sBE ] = Tv[ oT + j * st1 + j * st2 ];
		BETAv[ oBE + j * sBE + 1 ] = Tv[ oT + j * st1 + j * st2 + 1 ];
	}

	// If IHI < ILO, skip QZ steps
	if ( ihi < ilo ) {
		return setEigenvaluesBelow( ilo, N, ilschr, ilz, safmin, H, sh1, sh2, offsetH, T, st1, st2, offsetT, ALPHA, strideALPHA, offsetALPHA, BETA, strideBETA, offsetBETA, Z, sz1, sz2, offsetZ, signbc );
	}

	// MAIN QZ ITERATION LOOP
	ilast = ihi - 1; // 0-based (Fortran ILAST=IHI is 1-based)
	if ( ilschr ) {
		ifrstm = 0;     // 0-based (Fortran IFRSTM=1)
		ilastm = N - 1; // 0-based (Fortran ILASTM=N)
	} else {
		ifrstm = ilo - 1; // 0-based
		ilastm = ihi - 1; // 0-based
	}
	iiter = 0;
	eshift[ 0 ] = ZERO;
	eshift[ 1 ] = ZERO;
	maxit = 30 * ( ihi - ilo + 1 );
	done = false;

	for ( jiter = 1; jiter <= maxit; jiter++ ) {
		// ================================================================
		// Split the matrix if possible.
		// ================================================================
		// Special case: ILAST == ILO-1 (0-based: ilast == ilo-1)
		if ( ilast === ilo - 1 ) {
			// GO TO 60: deflate
			deflateAndExtract();
			if ( done ) {
				return info;
			}
			continue;
		}

		// Check if H(ILAST, ILAST-1) is small
		if ( cabs1At( Hv, oH + ilast * sh1 + ( ilast - 1 ) * sh2 ) <=
			Math.max( safmin, ulp * ( cabs1At( Hv, oH + ilast * sh1 + ilast * sh2 ) +
			cabs1At( Hv, oH + ( ilast - 1 ) * sh1 + ( ilast - 1 ) * sh2 ) ) ) ) {
			Hv[ oH + ilast * sh1 + ( ilast - 1 ) * sh2 ] = ZERO;
			Hv[ oH + ilast * sh1 + ( ilast - 1 ) * sh2 + 1 ] = ZERO;
			deflateAndExtract();
			if ( done ) {
				return info;
			}
			continue;
		}

		// Check if T(ILAST, ILAST) is small
		if ( cabs( Tv, oT + ilast * st1 + ilast * st2 ) <= btol ) {
			Tv[ oT + ilast * st1 + ilast * st2 ] = ZERO;
			Tv[ oT + ilast * st1 + ilast * st2 + 1 ] = ZERO;
			handleZeroTdiag();
			if ( done ) {
				return info;
			}
			continue;
		}

		// General case: scan from ILAST-1 down to ILO
		if ( !scanAndProcess() ) {
			// Drop-through is "impossible" - set INFO=2*N+1
			info = 2 * N + 1;
			return info;
		}
		if ( done ) {
			return info;
		}
	}

	// Non-convergence
	info = ilast + 1; // Convert 0-based to 1-based for INFO
	return info;

	// ================================================================
	// Helper: set eigenvalues for indices 0..ilo-2 and return info=0
	// ================================================================
	function setEigenvaluesBelow() {
		var jj;
		var ab;
		for ( jj = 0; jj < ilo - 1; jj++ ) {
			ab = cabs( Tv, oT + jj * st1 + jj * st2 );
			if ( ab > safmin ) {
				cscaleConj( signbc, Tv, oT + jj * st1 + jj * st2, ab );
				Tv[ oT + jj * st1 + jj * st2 ] = ab;
				Tv[ oT + jj * st1 + jj * st2 + 1 ] = ZERO;
				if ( ilschr ) {
					zscal( jj, signbc, T, strideT1, offsetT + jj * strideT2 );
					zscal( jj + 1, signbc, H, strideH1, offsetH + jj * strideH2 );
				} else {
					zscal( 1, signbc, H, 1, offsetH + jj * strideH1 + jj * strideH2 );
				}
				if ( ilz ) {
					zscal( N, signbc, Z, strideZ1, offsetZ + jj * strideZ2 );
				}
			} else {
				Tv[ oT + jj * st1 + jj * st2 ] = ZERO;
				Tv[ oT + jj * st1 + jj * st2 + 1 ] = ZERO;
			}
			ALPHAv[ oAL + jj * sAL ] = Hv[ oH + jj * sh1 + jj * sh2 ];
			ALPHAv[ oAL + jj * sAL + 1 ] = Hv[ oH + jj * sh1 + jj * sh2 + 1 ];
			BETAv[ oBE + jj * sBE ] = Tv[ oT + jj * st1 + jj * st2 ];
			BETAv[ oBE + jj * sBE + 1 ] = Tv[ oT + jj * st1 + jj * st2 + 1 ];
		}
		return 0;
	}

	// ================================================================
	// Handle T(ILAST,ILAST)=0: clear H(ILAST,ILAST-1) then deflate
	// (Fortran label 50 -> 60)
	// ================================================================
	function handleZeroTdiag() {
		var idx1;
		var idx2;
		// CTEMP = H(ILAST, ILAST)
		idx1 = oH + ilast * sh1 + ilast * sh2;
		f[ 0 ] = Hv[ idx1 ];
		f[ 1 ] = H[ idx1 + 1 ];
		// G = H(ILAST, ILAST-1)
		idx2 = oH + ilast * sh1 + ( ilast - 1 ) * sh2;
		g[ 0 ] = Hv[ idx2 ];
		g[ 1 ] = H[ idx2 + 1 ];

		zlartg( f, g, out );
		c = out[ 0 ];
		s[ 0 ] = out[ 1 ];
		s[ 1 ] = out[ 2 ];

		// H(ILAST, ILAST) = R
		Hv[ idx1 ] = out[ 3 ];
		H[ idx1 + 1 ] = out[ 4 ];
		// H(ILAST, ILAST-1) = 0
		Hv[ idx2 ] = ZERO;
		H[ idx2 + 1 ] = ZERO;

		// ZROT(ILAST-IFRSTM, H(IFRSTM,ILAST), 1, H(IFRSTM,ILAST-1), 1, C, S)
		// In Fortran: column-wise rotation, iterating over rows from IFRSTM to ILAST-1
		// Count: ILAST - IFRSTM (Fortran 1-based ILAST-IFRSTM)
		// 0-based: ilast - ifrstm
		zrot(
			ilast - ifrstm,
			H, strideH1, offsetH + ifrstm * strideH1 + ilast * strideH2,
			H, strideH1, offsetH + ifrstm * strideH1 + ( ilast - 1 ) * strideH2,
			c, s
		);
		// ZROT(ILAST-IFRSTM, T(IFRSTM,ILAST), 1, T(IFRSTM,ILAST-1), 1, C, S)
		zrot(
			ilast - ifrstm,
			T, strideT1, offsetT + ifrstm * strideT1 + ilast * strideT2,
			T, strideT1, offsetT + ifrstm * strideT1 + ( ilast - 1 ) * strideT2,
			c, s
		);
		if ( ilz ) {
			zrot(
				N,
				Z, rz1, offsetZ + ilast * strideZ2,
				Z, rz1, offsetZ + ( ilast - 1 ) * strideZ2,
				c, s
			);
		}

		// Fall through to deflate (label 60)
		deflateAndExtract();
	}

	// ================================================================
	// Deflate: standardize B diagonal, extract eigenvalue
	// (Fortran label 60)
	// ================================================================
	function deflateAndExtract() {
		var ab;
		var idx;
		ab = cabs( Tv, oT + ilast * st1 + ilast * st2 );
		if ( ab > safmin ) {
			cscaleConj( signbc, Tv, oT + ilast * st1 + ilast * st2, ab );
			Tv[ oT + ilast * st1 + ilast * st2 ] = ab;
			Tv[ oT + ilast * st1 + ilast * st2 + 1 ] = ZERO;
			if ( ilschr ) {
				// ZSCAL(ILAST-IFRSTM, SIGNBC, T(IFRSTM,ILAST), 1)
				zscal( ilast - ifrstm, signbc, T, strideT1, offsetT + ifrstm * strideT1 + ilast * strideT2 );
				// ZSCAL(ILAST+1-IFRSTM, SIGNBC, H(IFRSTM,ILAST), 1)
				zscal( ilast + 1 - ifrstm, signbc, H, strideH1, offsetH + ifrstm * strideH1 + ilast * strideH2 );
			} else {
				zscal( 1, signbc, H, 1, offsetH + ilast * strideH1 + ilast * strideH2 );
			}
			if ( ilz ) {
				zscal( N, signbc, Z, strideZ1, offsetZ + ilast * strideZ2 );
			}
		} else {
			Tv[ oT + ilast * st1 + ilast * st2 ] = ZERO;
			Tv[ oT + ilast * st1 + ilast * st2 + 1 ] = ZERO;
		}

		// ALPHA(ILAST) = H(ILAST,ILAST), BETA(ILAST) = T(ILAST,ILAST)
		idx = oH + ilast * sh1 + ilast * sh2;
		ALPHAv[ oAL + ilast * sAL ] = Hv[ idx ];
		ALPHAv[ oAL + ilast * sAL + 1 ] = H[ idx + 1 ];
		idx = oT + ilast * st1 + ilast * st2;
		BETAv[ oBE + ilast * sBE ] = Tv[ idx ];
		BETAv[ oBE + ilast * sBE + 1 ] = T[ idx + 1 ];

		// Next block
		ilast -= 1;
		if ( ilast < ilo - 1 ) {
			// Successful completion: GO TO 190
			info = setEigenvaluesBelow();
			done = true;
			return;
		}

		// Reset counters
		iiter = 0;
		eshift[ 0 ] = ZERO;
		eshift[ 1 ] = ZERO;
		if ( !ilschr ) {
			ilastm = ilast;
			if ( ifrstm > ilast ) {
				ifrstm = ilo - 1; // 0-based
			}
		}
		// GO TO 160 (continue to next iteration of main loop)
	}

	// ================================================================
	// Scan J from ILAST-1 down to ILO (0-based), perform tests, and
	// route to appropriate handler. Returns true if processing happened,
	// false for "drop-through" (impossible in theory).
	// ================================================================
	function scanAndProcess() {
		var found;
		var jj;
		// DO 40 J = ILAST-1, ILO, -1 (Fortran 1-based)
		// 0-based: j from ilast-1 down to ilo-1
		found = false;
		for ( jj = ilast - 1; jj >= ilo - 1; jj-- ) {
			// Test 1: H(j, j-1) = 0 or j = ILO (0-based: jj == ilo-1)
			if ( jj === ilo - 1 ) {
				ilazro = true;
			} else {
				if ( cabs1At( Hv, oH + jj * sh1 + ( jj - 1 ) * sh2 ) <=
					Math.max( safmin, ulp * ( cabs1At( Hv, oH + jj * sh1 + jj * sh2 ) +
					cabs1At( Hv, oH + ( jj - 1 ) * sh1 + ( jj - 1 ) * sh2 ) ) ) ) {
					Hv[ oH + jj * sh1 + ( jj - 1 ) * sh2 ] = ZERO;
					Hv[ oH + jj * sh1 + ( jj - 1 ) * sh2 + 1 ] = ZERO;
					ilazro = true;
				} else {
					ilazro = false;
				}
			}

			// Test 2: T(j, j) = 0
			if ( cabs( Tv, oT + jj * st1 + jj * st2 ) < btol ) {
				Tv[ oT + jj * st1 + jj * st2 ] = ZERO;
				Tv[ oT + jj * st1 + jj * st2 + 1 ] = ZERO;

				// Test 1a: two consecutive small subdiags
				ilazr2 = false;
				if ( !ilazro ) {
					if ( cabs1At( Hv, oH + jj * sh1 + ( jj - 1 ) * sh2 ) *
						( ascale * cabs1At( Hv, oH + ( jj + 1 ) * sh1 + jj * sh2 ) ) <=
						cabs1At( Hv, oH + jj * sh1 + jj * sh2 ) * ( ascale * atol ) ) {
						ilazr2 = true;
					}
				}

				if ( ilazro || ilazr2 ) {
					// Both tests pass: split 1x1 block from top (DO 20 loop)
					found = handleBothTestsPass( jj );
					if ( found ) {
						return true;
					}
					// If loop completes without break, go to label 50
					handleZeroTdiag();
					return true;
				}
				// Only test 2 passed: chase zero to T(ILAST,ILAST)
				chaseZeroToBottom( jj );
				handleZeroTdiag();
				return true;
			} else if ( ilazro ) {
				// Only test 1 passed: ifirst = j, go to QZ step
				ifirst = jj;
				doQZStep();
				return true;
			}
			// Neither test passed, continue scanning
		}
		return false; // drop-through (should be impossible)
	}

	// ================================================================
	// Both tests pass: split 1x1 block (DO 20 loop in Fortran)
	// Returns true if found a deflation/QZ entry point, false if
	// loop completed (fall to label 50).
	// ================================================================
	function handleBothTestsPass( jj ) {
		var kk;
		var idx1;
		var idx2;
		for ( kk = jj; kk <= ilast - 1; kk++ ) {
			// CTEMP = H(JCH, JCH)
			idx1 = oH + kk * sh1 + kk * sh2;
			f[ 0 ] = Hv[ idx1 ];
			f[ 1 ] = H[ idx1 + 1 ];
			// G = H(JCH+1, JCH)
			idx2 = oH + ( kk + 1 ) * sh1 + kk * sh2;
			g[ 0 ] = Hv[ idx2 ];
			g[ 1 ] = H[ idx2 + 1 ];

			zlartg( f, g, out );
			c = out[ 0 ];
			s[ 0 ] = out[ 1 ];
			s[ 1 ] = out[ 2 ];

			// H(JCH, JCH) = R
			Hv[ idx1 ] = out[ 3 ];
			H[ idx1 + 1 ] = out[ 4 ];
			// H(JCH+1, JCH) = 0
			Hv[ idx2 ] = ZERO;
			H[ idx2 + 1 ] = ZERO;

			// ZROT(ILASTM-JCH, H(JCH,JCH+1), LDH, H(JCH+1,JCH+1), LDH, C, S)
			// Row rotation across columns JCH+1..ILASTM
			// Fortran: ILASTM-JCH elements, stride=LDH (column stride)
			// 0-based: ilastm - kk elements
			zrot(
				ilastm - kk,
				H, strideH2, offsetH + kk * strideH1 + ( kk + 1 ) * strideH2,
				H, strideH2, offsetH + ( kk + 1 ) * strideH1 + ( kk + 1 ) * strideH2,
				c, s
			);
			// Same for T
			zrot(
				ilastm - kk,
				T, strideT2, offsetT + kk * strideT1 + ( kk + 1 ) * strideT2,
				T, strideT2, offsetT + ( kk + 1 ) * strideT1 + ( kk + 1 ) * strideT2,
				c, s
			);
			// Q rotation with conj(s)
			if ( ilq ) {
				s[ 1 ] = -s[ 1 ]; // conjugate
				zrot(
					N,
					Q, rq1, offsetQ + kk * strideQ2,
					Q, rq1, offsetQ + ( kk + 1 ) * strideQ2,
					c, s
				);
				s[ 1 ] = -s[ 1 ]; // restore
			}
			// ILAZR2 adjustment
			if ( ilazr2 ) {
				idx1 = oH + kk * sh1 + ( kk - 1 ) * sh2;
				Hv[ idx1 ] = Hv[ idx1 ] * c;
				H[ idx1 + 1 ] = H[ idx1 + 1 ] * c;
			}
			ilazr2 = false;

			// Check convergence
			if ( cabs1At( Tv, oT + ( kk + 1 ) * st1 + ( kk + 1 ) * strideT2 ) >= btol ) {
				if ( kk + 1 >= ilast ) {
					// GO TO 60
					deflateAndExtract();
					return true;
				}
				ifirst = kk + 1;
				doQZStep();
				return true;
			}
			Tv[ oT + ( kk + 1 ) * st1 + ( kk + 1 ) * strideT2 ] = ZERO;
			Tv[ oT + ( kk + 1 ) * st1 + ( kk + 1 ) * strideT2 + 1 ] = ZERO;
		}
		return false; // fall to label 50
	}

	// ================================================================
	// Chase the zero in T to T(ILAST,ILAST) (DO 30 loop in Fortran)
	// ================================================================
	function chaseZeroToBottom( jj ) {
		var kk;
		var idx1;
		var idx2;
		for ( kk = jj; kk <= ilast - 1; kk++ ) {
			// CTEMP = T(JCH, JCH+1)
			idx1 = oT + kk * st1 + ( kk + 1 ) * strideT2;
			f[ 0 ] = Tv[ idx1 ];
			f[ 1 ] = T[ idx1 + 1 ];
			// G = T(JCH+1, JCH+1)
			idx2 = oT + ( kk + 1 ) * st1 + ( kk + 1 ) * strideT2;
			g[ 0 ] = Tv[ idx2 ];
			g[ 1 ] = T[ idx2 + 1 ];

			zlartg( f, g, out );
			c = out[ 0 ];
			s[ 0 ] = out[ 1 ];
			s[ 1 ] = out[ 2 ];

			// T(JCH, JCH+1) = R
			Tv[ idx1 ] = out[ 3 ];
			T[ idx1 + 1 ] = out[ 4 ];
			// T(JCH+1, JCH+1) = 0
			Tv[ idx2 ] = ZERO;
			T[ idx2 + 1 ] = ZERO;

			// IF(JCH < ILASTM-1) ZROT(ILASTM-JCH-1, T(JCH,JCH+2), LDT, T(JCH+1,JCH+2), LDT, C, S)
			// 0-based: kk < ilastm-1
			if ( kk < ilastm - 1 ) {
				zrot(
					ilastm - kk - 1,
					T, strideT2, offsetT + kk * strideT1 + ( kk + 2 ) * strideT2,
					T, strideT2, offsetT + ( kk + 1 ) * strideT1 + ( kk + 2 ) * strideT2,
					c, s
				);
			}
			// ZROT(ILASTM-JCH+2, H(JCH,JCH-1), LDH, H(JCH+1,JCH-1), LDH, C, S)
			// Fortran 1-based count: ILASTM-JCH+2
			// 0-based count: (ilastm+1) - (kk) + 1 = ilastm - kk + 2
			// Wait: Fortran JCH is 1-based, ILASTM is 1-based
			// ILASTM-JCH+2 in 1-based = (ilastm+1) - (kk+1) + 2 = ilastm - kk + 2
			zrot(
				ilastm - kk + 2,
				H, strideH2, offsetH + kk * strideH1 + ( kk - 1 ) * strideH2,
				H, strideH2, offsetH + ( kk + 1 ) * strideH1 + ( kk - 1 ) * strideH2,
				c, s
			);
			// Q rotation
			if ( ilq ) {
				s[ 1 ] = -s[ 1 ]; // conjugate
				zrot(
					N,
					Q, rq1, offsetQ + kk * strideQ2,
					Q, rq1, offsetQ + ( kk + 1 ) * strideQ2,
					c, s
				);
				s[ 1 ] = -s[ 1 ]; // restore
			}

			// Step 2: eliminate H(JCH+1, JCH-1)
			// CTEMP = H(JCH+1, JCH)
			idx1 = oH + ( kk + 1 ) * sh1 + kk * sh2;
			f[ 0 ] = Hv[ idx1 ];
			f[ 1 ] = H[ idx1 + 1 ];
			// G = H(JCH+1, JCH-1)
			idx2 = oH + ( kk + 1 ) * sh1 + ( kk - 1 ) * sh2;
			g[ 0 ] = Hv[ idx2 ];
			g[ 1 ] = H[ idx2 + 1 ];

			zlartg( f, g, out );
			c = out[ 0 ];
			s[ 0 ] = out[ 1 ];
			s[ 1 ] = out[ 2 ];

			// H(JCH+1, JCH) = R
			Hv[ idx1 ] = out[ 3 ];
			H[ idx1 + 1 ] = out[ 4 ];
			// H(JCH+1, JCH-1) = 0
			Hv[ idx2 ] = ZERO;
			H[ idx2 + 1 ] = ZERO;

			// ZROT(JCH+1-IFRSTM, H(IFRSTM,JCH), 1, H(IFRSTM,JCH-1), 1, C, S)
			// Fortran 1-based: JCH+1-IFRSTM elements
			// 0-based: (kk+1) + 1 - (ifrstm+1) = kk + 1 - ifrstm
			zrot(
				kk + 1 - ifrstm,
				H, strideH1, offsetH + ifrstm * strideH1 + kk * strideH2,
				H, strideH1, offsetH + ifrstm * strideH1 + ( kk - 1 ) * strideH2,
				c, s
			);
			// ZROT(JCH-IFRSTM, T(IFRSTM,JCH), 1, T(IFRSTM,JCH-1), 1, C, S)
			// 0-based: kk - ifrstm
			zrot(
				kk - ifrstm,
				T, strideT1, offsetT + ifrstm * strideT1 + kk * strideT2,
				T, strideT1, offsetT + ifrstm * strideT1 + ( kk - 1 ) * strideT2,
				c, s
			);
			// Z rotation
			if ( ilz ) {
				zrot(
					N,
					Z, rz1, offsetZ + kk * strideZ2,
					Z, rz1, offsetZ + ( kk - 1 ) * strideZ2,
					c, s
				);
			}
		}
	}

	// ================================================================
	// QZ step (Fortran label 70)
	// ================================================================
	function doQZStep() {
		var divi;
		var divr;
		var t1r;
		var t1i;
		var t2r;
		var t2i;
		var jj;
		var idx;

		iiter += 1;
		if ( !ilschr ) {
			ifrstm = ifirst;
		}

		// Compute shift
		if ( ( iiter % 10 ) !== 0 ) {
			// Wilkinson shift
			computeWilkinsonShift();
		} else {
			// Exceptional shift
			if ( ( iiter % 20 ) === 0 &&
				bscale * cabs1At( Tv, oT + ilast * st1 + ilast * st2 ) > safmin ) {
				// eshift += (ascale*H(ilast,ilast)) / (bscale*T(ilast,ilast))
				divr = bscale * Tv[ oT + ilast * st1 + ilast * st2 ];
				divi = bscale * Tv[ oT + ilast * st1 + ilast * st2 + 1 ];
				t1r = ascale * Hv[ oH + ilast * sh1 + ilast * sh2 ];
				t1i = ascale * Hv[ oH + ilast * sh1 + ilast * sh2 + 1 ];
				cdivInline( ctemp, t1r, t1i, divr, divi );
				eshift[ 0 ] += ctemp[ 0 ];
				eshift[ 1 ] += ctemp[ 1 ];
			} else {
				// eshift += (ascale*H(ilast,ilast-1)) / (bscale*T(ilast-1,ilast-1))
				divr = bscale * Tv[ oT + ( ilast - 1 ) * st1 + ( ilast - 1 ) * st2 ];
				divi = bscale * Tv[ oT + ( ilast - 1 ) * st1 + ( ilast - 1 ) * st2 + 1 ];
				t1r = ascale * Hv[ oH + ilast * sh1 + ( ilast - 1 ) * sh2 ];
				t1i = ascale * Hv[ oH + ilast * sh1 + ( ilast - 1 ) * sh2 + 1 ];
				cdivInline( ctemp, t1r, t1i, divr, divi );
				eshift[ 0 ] += ctemp[ 0 ];
				eshift[ 1 ] += ctemp[ 1 ];
			}
			shift[ 0 ] = eshift[ 0 ];
			shift[ 1 ] = eshift[ 1 ];
		}

		// Check for two consecutive small subdiagonals
		istart = ifirst;
		// DO 80 J = ILAST-1, IFIRST+1, -1 (Fortran 1-based)
		// 0-based: jj from ilast-1 down to ifirst+1
		for ( jj = ilast - 1; jj >= ifirst + 1; jj-- ) {
			istart = jj;
			// CTEMP = ASCALE*H(J,J) - SHIFT*(BSCALE*T(J,J))
			idx = oH + jj * sh1 + jj * sh2;
			t1r = ascale * Hv[ idx ] - shift[ 0 ] * ( bscale * Tv[ oT + jj * st1 + jj * st2 ] ) + shift[ 1 ] * ( bscale * Tv[ oT + jj * st1 + jj * st2 + 1 ] );
			t1i = ascale * H[ idx + 1 ] - shift[ 0 ] * ( bscale * Tv[ oT + jj * st1 + jj * st2 + 1 ] ) - shift[ 1 ] * ( bscale * Tv[ oT + jj * st1 + jj * st2 ] );
			temp = Math.abs( t1r ) + Math.abs( t1i );
			temp2 = ascale * cabs1At( Hv, oH + ( jj + 1 ) * sh1 + jj * sh2 );
			tempr = Math.max( temp, temp2 );
			if ( tempr < ONE && tempr !== ZERO ) {
				temp = temp / tempr;
				temp2 = temp2 / tempr;
			}
			if ( cabs1At( Hv, oH + jj * sh1 + ( jj - 1 ) * sh2 ) * temp2 <= temp * atol ) {
				// GO TO 90
				break;
			}
		}
		if ( jj < ifirst + 1 ) {
			// Fell through loop: ISTART = IFIRST
			istart = ifirst;
		}

		// Compute CTEMP at ISTART
		idx = oH + istart * sh1 + istart * sh2;
		ctemp[ 0 ] = ascale * Hv[ idx ] - shift[ 0 ] * ( bscale * Tv[ oT + istart * st1 + istart * st2 ] ) + shift[ 1 ] * ( bscale * Tv[ oT + istart * st1 + istart * st2 + 1 ] );
		ctemp[ 1 ] = ascale * H[ idx + 1 ] - shift[ 0 ] * ( bscale * Tv[ oT + istart * st1 + istart * st2 + 1 ] ) - shift[ 1 ] * ( bscale * Tv[ oT + istart * st1 + istart * st2 ] );

		// Initial Q: CTEMP2 = ASCALE * H(ISTART+1, ISTART)
		idx = oH + ( istart + 1 ) * sh1 + istart * sh2;
		ctemp2[ 0 ] = ascale * Hv[ idx ];
		ctemp2[ 1 ] = ascale * H[ idx + 1 ];

		// ZLARTG(CTEMP, CTEMP2, C, S, CTEMP3)
		zlartg( ctemp, ctemp2, out );
		c = out[ 0 ];
		s[ 0 ] = out[ 1 ];
		s[ 1 ] = out[ 2 ];

		// QZ sweep: DO 150 J = ISTART, ILAST-1
		doQZSweep();
	}

	// ================================================================
	// Wilkinson shift computation
	// ================================================================
	function computeWilkinsonShift() {
		var divr;
		var divi;
		var idx;
		var t1r;
		var t1i;
		var t2r;
		var t2i;
		var tr;
		var ti;

		// U12 = (BSCALE*T(ILAST-1,ILAST)) / (BSCALE*T(ILAST,ILAST))
		idx = oT + ( ilast - 1 ) * st1 + ilast * st2;
		t1r = bscale * Tv[ idx ];
		t1i = bscale * T[ idx + 1 ];
		idx = oT + ilast * st1 + ilast * st2;
		divr = bscale * Tv[ idx ];
		divi = bscale * T[ idx + 1 ];
		cdivInline( u12, t1r, t1i, divr, divi );

		// AD11 = (ASCALE*H(ILAST-1,ILAST-1)) / (BSCALE*T(ILAST-1,ILAST-1))
		idx = oH + ( ilast - 1 ) * sh1 + ( ilast - 1 ) * sh2;
		t1r = ascale * Hv[ idx ];
		t1i = ascale * H[ idx + 1 ];
		idx = oT + ( ilast - 1 ) * st1 + ( ilast - 1 ) * st2;
		divr = bscale * Tv[ idx ];
		divi = bscale * T[ idx + 1 ];
		cdivInline( ad11, t1r, t1i, divr, divi );

		// AD21 = (ASCALE*H(ILAST,ILAST-1)) / (BSCALE*T(ILAST-1,ILAST-1))
		idx = oH + ilast * sh1 + ( ilast - 1 ) * sh2;
		t1r = ascale * Hv[ idx ];
		t1i = ascale * H[ idx + 1 ];
		// denominator same as AD11
		cdivInline( ad21, t1r, t1i, divr, divi );

		// AD12 = (ASCALE*H(ILAST-1,ILAST)) / (BSCALE*T(ILAST,ILAST))
		idx = oH + ( ilast - 1 ) * sh1 + ilast * sh2;
		t1r = ascale * Hv[ idx ];
		t1i = ascale * H[ idx + 1 ];
		idx = oT + ilast * st1 + ilast * st2;
		divr = bscale * Tv[ idx ];
		divi = bscale * T[ idx + 1 ];
		cdivInline( ad12, t1r, t1i, divr, divi );

		// AD22 = (ASCALE*H(ILAST,ILAST)) / (BSCALE*T(ILAST,ILAST))
		idx = oH + ilast * sh1 + ilast * sh2;
		t1r = ascale * Hv[ idx ];
		t1i = ascale * H[ idx + 1 ];
		// same denominator
		cdivInline( ad22, t1r, t1i, divr, divi );

		// ABI22 = AD22 - U12*AD21
		abi22[ 0 ] = ad22[ 0 ] - ( u12[ 0 ] * ad21[ 0 ] - u12[ 1 ] * ad21[ 1 ] );
		abi22[ 1 ] = ad22[ 1 ] - ( u12[ 0 ] * ad21[ 1 ] + u12[ 1 ] * ad21[ 0 ] );

		// ABI12 = AD12 - U12*AD11
		abi12[ 0 ] = ad12[ 0 ] - ( u12[ 0 ] * ad11[ 0 ] - u12[ 1 ] * ad11[ 1 ] );
		abi12[ 1 ] = ad12[ 1 ] - ( u12[ 0 ] * ad11[ 1 ] + u12[ 1 ] * ad11[ 0 ] );

		// SHIFT = ABI22
		shift[ 0 ] = abi22[ 0 ];
		shift[ 1 ] = abi22[ 1 ];

		// CTEMP = SQRT(ABI12) * SQRT(AD21)
		// Complex sqrt
		csqrt( ctemp, abi12 );
		csqrt( ctemp2, ad21 );
		// ctemp3 = ctemp * ctemp2
		ctemp3[ 0 ] = ctemp[ 0 ] * ctemp2[ 0 ] - ctemp[ 1 ] * ctemp2[ 1 ];
		ctemp3[ 1 ] = ctemp[ 0 ] * ctemp2[ 1 ] + ctemp[ 1 ] * ctemp2[ 0 ];
		temp = Math.abs( ctemp3[ 0 ] ) + Math.abs( ctemp3[ 1 ] );

		if ( ctemp3[ 0 ] !== ZERO || ctemp3[ 1 ] !== ZERO ) {
			// X = HALF*(AD11 - SHIFT)
			x[ 0 ] = HALF * ( ad11[ 0 ] - shift[ 0 ] );
			x[ 1 ] = HALF * ( ad11[ 1 ] - shift[ 1 ] );
			temp2 = Math.abs( x[ 0 ] ) + Math.abs( x[ 1 ] );
			temp = Math.max( temp, temp2 );

			// Y = TEMP * SQRT((X/TEMP)**2 + (CTEMP3/TEMP)**2)
			// Complex: (X/TEMP)**2
			t1r = x[ 0 ] / temp;
			t1i = x[ 1 ] / temp;
			// t1^2
			tr = t1r * t1r - t1i * t1i;
			ti = 2.0 * t1r * t1i;
			// (CTEMP3/TEMP)**2
			t2r = ctemp3[ 0 ] / temp;
			t2i = ctemp3[ 1 ] / temp;
			divr = t2r * t2r - t2i * t2i;
			divi = 2.0 * t2r * t2i;
			// sum
			tr += divr;
			ti += divi;
			// sqrt of sum
			ctemp[ 0 ] = tr;
			ctemp[ 1 ] = ti;
			csqrt( y, ctemp );
			// Y = TEMP * Y
			y[ 0 ] *= temp;
			y[ 1 ] *= temp;

			if ( temp2 > ZERO ) {
				// IF(DBLE(X/TEMP2)*DBLE(Y) + DIMAG(X/TEMP2)*DIMAG(Y) < 0) Y = -Y
				t1r = x[ 0 ] / temp2;
				t1i = x[ 1 ] / temp2;
				if ( t1r * y[ 0 ] + t1i * y[ 1 ] < ZERO ) {
					y[ 0 ] = -y[ 0 ];
					y[ 1 ] = -y[ 1 ];
				}
			}
			// SHIFT = SHIFT - CTEMP3*ZLADIV(CTEMP3, X+Y)
			// numerator = ctemp3, denominator = x+y
			t1r = x[ 0 ] + y[ 0 ];
			t1i = x[ 1 ] + y[ 1 ];
			f[ 0 ] = ctemp3[ 0 ];
			f[ 1 ] = ctemp3[ 1 ];
			g[ 0 ] = t1r;
			g[ 1 ] = t1i;
			zladiv( f, g, ctemp );
			// shift -= ctemp3 * ctemp
			shift[ 0 ] -= ( ctemp3[ 0 ] * ctemp[ 0 ] - ctemp3[ 1 ] * ctemp[ 1 ] );
			shift[ 1 ] -= ( ctemp3[ 0 ] * ctemp[ 1 ] + ctemp3[ 1 ] * ctemp[ 0 ] );
		}
	}

	// ================================================================
	// QZ sweep: DO 150 J = ISTART, ILAST-1
	// ================================================================
	function doQZSweep() {
		var idx1;
		var idx2;
		var cr;
		var ci;
		var jj;
		var kk;
		var mn;

		for ( jj = istart; jj <= ilast - 1; jj++ ) {
			if ( jj > istart ) {
				// CTEMP = H(J, J-1)
				idx1 = oH + jj * sh1 + ( jj - 1 ) * sh2;
				f[ 0 ] = Hv[ idx1 ];
				f[ 1 ] = H[ idx1 + 1 ];
				// G = H(J+1, J-1)
				idx2 = oH + ( jj + 1 ) * sh1 + ( jj - 1 ) * sh2;
				g[ 0 ] = Hv[ idx2 ];
				g[ 1 ] = H[ idx2 + 1 ];

				zlartg( f, g, out );
				c = out[ 0 ];
				s[ 0 ] = out[ 1 ];
				s[ 1 ] = out[ 2 ];

				// H(J, J-1) = R
				Hv[ idx1 ] = out[ 3 ];
				H[ idx1 + 1 ] = out[ 4 ];
				// H(J+1, J-1) = 0
				Hv[ idx2 ] = ZERO;
				H[ idx2 + 1 ] = ZERO;
			}

			// Apply from left to H: DO 100 JC = J, ILASTM
			for ( kk = jj; kk <= ilastm; kk++ ) {
				idx1 = oH + jj * sh1 + kk * sh2;
				idx2 = oH + ( jj + 1 ) * sh1 + kk * sh2;
				// CTEMP = C*H(J,JC) + S*H(J+1,JC)
				cr = c * Hv[ idx1 ] + s[ 0 ] * Hv[ idx2 ] - s[ 1 ] * H[ idx2 + 1 ];
				ci = c * H[ idx1 + 1 ] + s[ 0 ] * H[ idx2 + 1 ] + s[ 1 ] * Hv[ idx2 ];
				// H(J+1,JC) = -conj(S)*H(J,JC) + C*H(J+1,JC)
				Hv[ idx2 ] = -s[ 0 ] * Hv[ idx1 ] - s[ 1 ] * H[ idx1 + 1 ] + c * Hv[ idx2 ];
				H[ idx2 + 1 ] = -s[ 0 ] * H[ idx1 + 1 ] + s[ 1 ] * Hv[ idx1 ] + c * H[ idx2 + 1 ];
				Hv[ idx1 ] = cr;
				H[ idx1 + 1 ] = ci;

				// Same for T
				idx1 = oT + jj * st1 + kk * st2;
				idx2 = oT + ( jj + 1 ) * st1 + kk * st2;
				cr = c * Tv[ idx1 ] + s[ 0 ] * Tv[ idx2 ] - s[ 1 ] * T[ idx2 + 1 ];
				ci = c * T[ idx1 + 1 ] + s[ 0 ] * T[ idx2 + 1 ] + s[ 1 ] * Tv[ idx2 ];
				Tv[ idx2 ] = -s[ 0 ] * Tv[ idx1 ] - s[ 1 ] * T[ idx1 + 1 ] + c * Tv[ idx2 ];
				T[ idx2 + 1 ] = -s[ 0 ] * T[ idx1 + 1 ] + s[ 1 ] * Tv[ idx1 ] + c * T[ idx2 + 1 ];
				Tv[ idx1 ] = cr;
				T[ idx1 + 1 ] = ci;
			}

			// Q rotation: IF(ILQ) DO 110 JR = 1, N
			if ( ilq ) {
				for ( kk = 0; kk < N; kk++ ) {
					idx1 = offsetQ + kk * sq1 + jj * sq2;
					idx2 = offsetQ + kk * sq1 + ( jj + 1 ) * sq2;
					// CTEMP = C*Q(JR,J) + conj(S)*Q(JR,J+1)
					cr = c * Q[ idx1 ] + s[ 0 ] * Q[ idx2 ] + s[ 1 ] * Q[ idx2 + 1 ];
					ci = c * Q[ idx1 + 1 ] + s[ 0 ] * Q[ idx2 + 1 ] - s[ 1 ] * Q[ idx2 ];
					// Q(JR,J+1) = -S*Q(JR,J) + C*Q(JR,J+1)
					Q[ idx2 ] = -s[ 0 ] * Q[ idx1 ] + s[ 1 ] * Q[ idx1 + 1 ] + c * Q[ idx2 ];
					Q[ idx2 + 1 ] = -s[ 0 ] * Q[ idx1 + 1 ] - s[ 1 ] * Q[ idx1 ] + c * Q[ idx2 + 1 ];
					Q[ idx1 ] = cr;
					Q[ idx1 + 1 ] = ci;
				}
			}

			// Step 2: eliminate T(J+1, J) with rotation from right
			// CTEMP = T(J+1, J+1)
			idx1 = oT + ( jj + 1 ) * st1 + ( jj + 1 ) * st2;
			f[ 0 ] = Tv[ idx1 ];
			f[ 1 ] = T[ idx1 + 1 ];
			// G = T(J+1, J)
			idx2 = oT + ( jj + 1 ) * st1 + jj * st2;
			g[ 0 ] = Tv[ idx2 ];
			g[ 1 ] = T[ idx2 + 1 ];

			zlartg( f, g, out );
			c = out[ 0 ];
			s[ 0 ] = out[ 1 ];
			s[ 1 ] = out[ 2 ];

			// T(J+1, J+1) = R
			Tv[ idx1 ] = out[ 3 ];
			T[ idx1 + 1 ] = out[ 4 ];
			// T(J+1, J) = 0
			Tv[ idx2 ] = ZERO;
			T[ idx2 + 1 ] = ZERO;

			// Apply from right to H: DO 120 JR = IFRSTM, MIN(J+2, ILAST)
			// Fortran 1-based: JR from IFRSTM to MIN(J+2, ILAST)
			// 0-based: kk from ifrstm to min(jj+2, ilast)
			mn = Math.min( jj + 2, ilast );
			for ( kk = ifrstm; kk <= mn; kk++ ) {
				idx1 = oH + kk * sh1 + ( jj + 1 ) * sh2;
				idx2 = oH + kk * sh1 + jj * sh2;
				// CTEMP = C*H(JR,J+1) + S*H(JR,J)
				cr = c * Hv[ idx1 ] + s[ 0 ] * Hv[ idx2 ] - s[ 1 ] * H[ idx2 + 1 ];
				ci = c * H[ idx1 + 1 ] + s[ 0 ] * H[ idx2 + 1 ] + s[ 1 ] * Hv[ idx2 ];
				// H(JR,J) = -conj(S)*H(JR,J+1) + C*H(JR,J)
				Hv[ idx2 ] = -s[ 0 ] * Hv[ idx1 ] - s[ 1 ] * H[ idx1 + 1 ] + c * Hv[ idx2 ];
				H[ idx2 + 1 ] = -s[ 0 ] * H[ idx1 + 1 ] + s[ 1 ] * Hv[ idx1 ] + c * H[ idx2 + 1 ];
				Hv[ idx1 ] = cr;
				H[ idx1 + 1 ] = ci;
			}

			// Apply from right to T: DO 130 JR = IFRSTM, J
			for ( kk = ifrstm; kk <= jj; kk++ ) {
				idx1 = oT + kk * st1 + ( jj + 1 ) * st2;
				idx2 = oT + kk * st1 + jj * st2;
				cr = c * Tv[ idx1 ] + s[ 0 ] * Tv[ idx2 ] - s[ 1 ] * T[ idx2 + 1 ];
				ci = c * T[ idx1 + 1 ] + s[ 0 ] * T[ idx2 + 1 ] + s[ 1 ] * Tv[ idx2 ];
				Tv[ idx2 ] = -s[ 0 ] * Tv[ idx1 ] - s[ 1 ] * T[ idx1 + 1 ] + c * Tv[ idx2 ];
				T[ idx2 + 1 ] = -s[ 0 ] * T[ idx1 + 1 ] + s[ 1 ] * Tv[ idx1 ] + c * T[ idx2 + 1 ];
				Tv[ idx1 ] = cr;
				T[ idx1 + 1 ] = ci;
			}

			// Z rotation: IF(ILZ) DO 140 JR = 1, N
			if ( ilz ) {
				for ( kk = 0; kk < N; kk++ ) {
					idx1 = offsetZ + kk * sz1 + ( jj + 1 ) * sz2;
					idx2 = offsetZ + kk * sz1 + jj * sz2;
					cr = c * Z[ idx1 ] + s[ 0 ] * Z[ idx2 ] - s[ 1 ] * Z[ idx2 + 1 ];
					ci = c * Z[ idx1 + 1 ] + s[ 0 ] * Z[ idx2 + 1 ] + s[ 1 ] * Z[ idx2 ];
					Z[ idx2 ] = -s[ 0 ] * Z[ idx1 ] - s[ 1 ] * Z[ idx1 + 1 ] + c * Z[ idx2 ];
					Z[ idx2 + 1 ] = -s[ 0 ] * Z[ idx1 + 1 ] + s[ 1 ] * Z[ idx1 ] + c * Z[ idx2 + 1 ];
					Z[ idx1 ] = cr;
					Z[ idx1 + 1 ] = ci;
				}
			}
		}
		// Label 160: continue to next iteration
	}
}

// ================================================================
// Utility functions
// ================================================================

/**
* Compute |Re(z)| + |Im(z)| at a given offset into an array.
*/
function cabs1At( arr, idx ) {
	return Math.abs( arr[ idx ] ) + Math.abs( arr[ idx + 1 ] );
}

/**
* Compute |z| = sqrt(re^2 + im^2) at a given offset into an array.
*/
function cabs( arr, idx ) {
	var re = arr[ idx ];
	var im = arr[ idx + 1 ];
	return Math.sqrt( re * re + im * im );
}

/**
* Compute signbc = conj(z / |z|) where z is at arr[idx].
*/
function cscaleConj( out, arr, idx, absval ) {
	out[ 0 ] = arr[ idx ] / absval;
	out[ 1 ] = -arr[ idx + 1 ] / absval;
}

/**
* Complex division: out = (ar + ai*i) / (br + bi*i)
*/
function cdivInline( out, ar, ai, br, bi ) {
	var r;
	var d;
	if ( Math.abs( bi ) <= Math.abs( br ) ) {
		r = bi / br;
		d = br + bi * r;
		out[ 0 ] = ( ar + ai * r ) / d;
		out[ 1 ] = ( ai - ar * r ) / d;
	} else {
		r = br / bi;
		d = bi + br * r;
		out[ 0 ] = ( ar * r + ai ) / d;
		out[ 1 ] = ( ai * r - ar ) / d;
	}
}

/**
* Complex square root: out = sqrt(z)
*/
function csqrt( out, z ) {
	var re = z[ 0 ];
	var im = z[ 1 ];
	var w;
	var r;
	if ( re === 0.0 && im === 0.0 ) {
		out[ 0 ] = 0.0;
		out[ 1 ] = 0.0;
		return;
	}
	r = Math.sqrt( re * re + im * im );
	w = Math.sqrt( ( Math.abs( re ) + r ) * 0.5 );
	if ( re >= 0.0 ) {
		out[ 0 ] = w;
		out[ 1 ] = im / ( 2.0 * w );
	} else {
		out[ 0 ] = Math.abs( im ) / ( 2.0 * w );
		out[ 1 ] = ( im >= 0.0 ) ? w : -w;
	}
}


// EXPORTS //

module.exports = zhgeqz;
