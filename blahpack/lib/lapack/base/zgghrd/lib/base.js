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

var zlartg = require( '../../zlartg/lib/base.js' );
var zlaset = require( '../../zlaset/lib/base.js' );
var zrot = require( '../../zrot/lib/base.js' );

// MAIN //

/**
* Reduce a pair of complex matrices (A, B) to generalized upper Hessenberg
* form using unitary transformations: Q^H * A * Z = H, Q^H * B * Z = T,
* where H is upper Hessenberg, T is upper triangular, and Q and Z are
* unitary.
*
* Complex elements are stored as interleaved real/imaginary pairs.
* Element (i, j) has real part at `offset + i*stride1 + j*stride2` and
* imaginary part at `offset + i*stride1 + j*stride2 + 1`.
*
* For complex matrices, stride1 and stride2 are in units of doubles.
* Column-major with LDA rows: stride1 = 2, stride2 = 2*LDA.
*
* @private
* @param {string} compq - 'N': do not compute Q; 'I': initialize Q to identity and compute; 'V': accumulate into Q
* @param {string} compz - 'N': do not compute Z; 'I': initialize Z to identity and compute; 'V': accumulate into Z
* @param {NonNegativeInteger} N - order of the matrices A and B
* @param {integer} ilo - ilo (1-based)
* @param {integer} ihi - ihi (1-based)
* @param {Float64Array} A - input/output matrix A (interleaved complex)
* @param {integer} strideA1 - stride of the first dimension of `A`
* @param {integer} strideA2 - stride of the second dimension of `A`
* @param {NonNegativeInteger} offsetA - starting index for `A`
* @param {Float64Array} B - input/output matrix B (interleaved complex)
* @param {integer} strideB1 - stride of the first dimension of `B`
* @param {integer} strideB2 - stride of the second dimension of `B`
* @param {NonNegativeInteger} offsetB - starting index for `B`
* @param {Float64Array} Q - input/output matrix Q (interleaved complex)
* @param {integer} strideQ1 - stride of the first dimension of `Q`
* @param {integer} strideQ2 - stride of the second dimension of `Q`
* @param {NonNegativeInteger} offsetQ - starting index for `Q`
* @param {Float64Array} Z - input/output matrix Z (interleaved complex)
* @param {integer} strideZ1 - stride of the first dimension of `Z`
* @param {integer} strideZ2 - stride of the second dimension of `Z`
* @param {NonNegativeInteger} offsetZ - starting index for `Z`
* @returns {integer} status code (0 = success)
*/
function zgghrd( compq, compz, N, ilo, ihi, A, strideA1, strideA2, offsetA, B, strideB1, strideB2, offsetB, Q, strideQ1, strideQ2, offsetQ, Z, strideZ1, strideZ2, offsetZ ) { // eslint-disable-line max-len, max-params
	var icompq;
	var icompz;
	var alpha;
	var beta;
	var jcol;
	var jrow;
	var ilq;
	var ilz;
	var out;
	var sa1;
	var sa2;
	var sb1;
	var sb2;
	var sq1;
	var sq2;
	var sz1;
	var sz2;
	var ra1; // zrot stride for A row-step (in complex elements)
	var ra2; // zrot stride for A col-step (in complex elements)
	var rb1;
	var rb2;
	var rq1;
	var rz1;
	var idx;
	var sr;
	var si;
	var c;
	var s;
	var f;
	var g;
	var j;

	// Decode COMPQ
	if ( compq === 'N' || compq === 'n' ) {
		ilq = false;
		icompq = 1;
	} else if ( compq === 'V' || compq === 'v' ) {
		ilq = true;
		icompq = 2;
	} else if ( compq === 'I' || compq === 'i' ) {
		ilq = true;
		icompq = 3;
	} else {
		icompq = 0;
	}

	// Decode COMPZ
	if ( compz === 'N' || compz === 'n' ) {
		ilz = false;
		icompz = 1;
	} else if ( compz === 'V' || compz === 'v' ) {
		ilz = true;
		icompz = 2;
	} else if ( compz === 'I' || compz === 'i' ) {
		ilz = true;
		icompz = 3;
	} else {
		icompz = 0;
	}

	// Parameter validation
	if ( icompq <= 0 ) {
		return -1;
	}
	if ( icompz <= 0 ) {
		return -2;
	}
	if ( N < 0 ) {
		return -3;
	}

	sa1 = strideA1;
	sa2 = strideA2;
	sb1 = strideB1;
	sb2 = strideB2;
	sq1 = strideQ1;
	sq2 = strideQ2;
	sz1 = strideZ1;
	sz2 = strideZ2;

	// zrot strides in complex elements (divide doubles-stride by 2)
	ra1 = sa1 / 2;
	ra2 = sa2 / 2;
	rb1 = sb1 / 2;
	rb2 = sb2 / 2;
	rq1 = sq1 / 2;
	rz1 = sz1 / 2;

	// Initialize Q to identity if requested
	alpha = new Float64Array( [ 0.0, 0.0 ] );
	beta = new Float64Array( [ 1.0, 0.0 ] );
	if ( icompq === 3 ) {
		zlaset( 'Full', N, N, alpha, beta, Q, sq1, sq2, offsetQ );
	}
	// Initialize Z to identity if requested
	if ( icompz === 3 ) {
		zlaset( 'Full', N, N, alpha, beta, Z, sz1, sz2, offsetZ );
	}

	// Quick return if N <= 1
	if ( N <= 1 ) {
		return 0;
	}

	// Zero out the lower triangle of B
	// Fortran: DO JCOL = 1, N-1; DO JROW = JCOL+1, N; B(JROW,JCOL)=0
	for ( jcol = 0; jcol < N - 1; jcol++ ) {
		for ( jrow = jcol + 1; jrow < N; jrow++ ) {
			idx = offsetB + jrow * sb1 + jcol * sb2;
			B[ idx ] = 0.0;
			B[ idx + 1 ] = 0.0;
		}
	}

	// Working arrays for zlartg
	f = new Float64Array( 2 );
	g = new Float64Array( 2 );
	out = new Float64Array( 5 ); // [c, s_re, s_im, r_re, r_im]
	s = new Float64Array( 2 );   // complex s for zrot

	// Main reduction loop
	// Fortran: DO JCOL = ILO, IHI-2 (1-based)
	// JS: jcol from ilo-1 to ihi-3 (0-based)
	for ( jcol = ilo - 1; jcol <= ihi - 3; jcol++ ) {
		// Fortran: DO JROW = IHI, JCOL+2, -1 (1-based)
		// JS: jrow from ihi-1 down to jcol+2 (0-based: JCOL+2 in 1-based = jcol+2 in 0-based)
		for ( jrow = ihi - 1; jrow >= jcol + 2; jrow-- ) {
			// -------------------------------------------------------
			// Step 1: Eliminate A(jrow, jcol) using a rotation applied
			// from the left.
			// Fortran: CTEMP = A(JROW-1, JCOL)
			// JS 0-based: A(jrow-1, jcol)
			// -------------------------------------------------------
			idx = offsetA + ( jrow - 1 ) * sa1 + jcol * sa2;
			f[ 0 ] = A[ idx ];
			f[ 1 ] = A[ idx + 1 ];

			idx = offsetA + jrow * sa1 + jcol * sa2;
			g[ 0 ] = A[ idx ];
			g[ 1 ] = A[ idx + 1 ];

			zlartg( f, g, out );
			c = out[ 0 ];
			sr = out[ 1 ];
			si = out[ 2 ];

			// A(JROW-1, JCOL) = R (the result from zlartg)
			idx = offsetA + ( jrow - 1 ) * sa1 + jcol * sa2;
			A[ idx ] = out[ 3 ];
			A[ idx + 1 ] = out[ 4 ];

			// A(JROW, JCOL) = 0
			idx = offsetA + jrow * sa1 + jcol * sa2;
			A[ idx ] = 0.0;
			A[ idx + 1 ] = 0.0;

			// Apply rotation to remaining columns of A:
			// Fortran: CALL ZROT(N-JCOL, A(JROW-1,JCOL+1), LDA, A(JROW,JCOL+1), LDA, C, S)
			// Iterates over columns (stride=LDA complex elements), N-JCOL elements (1-based)
			s[ 0 ] = sr;
			s[ 1 ] = si;
			zrot(
				N - ( jcol + 1 ),
				A, ra2, offsetA + ( jrow - 1 ) * sa1 + ( jcol + 1 ) * sa2,
				A, ra2, offsetA + jrow * sa1 + ( jcol + 1 ) * sa2,
				c, s
			);

			// Apply rotation to B:
			// Fortran: CALL ZROT(N+2-JROW, B(JROW-1,JROW-1), LDB, B(JROW,JROW-1), LDB, C, S)
			// Iterates over columns (stride=LDB complex elements)
			zrot(
				N + 1 - jrow,
				B, rb2, offsetB + ( jrow - 1 ) * sb1 + ( jrow - 1 ) * sb2,
				B, rb2, offsetB + jrow * sb1 + ( jrow - 1 ) * sb2,
				c, s
			);

			// Apply conjugate rotation to Q if needed:
			// Fortran: CALL ZROT(N, Q(1,JROW-1), 1, Q(1,JROW), 1, C, DCONJG(S))
			// Iterates over rows (stride=1 complex element)
			if ( ilq ) {
				s[ 0 ] = sr;
				s[ 1 ] = -si; // conjugate
				zrot(
					N,
					Q, rq1, offsetQ + ( jrow - 1 ) * sq2,
					Q, rq1, offsetQ + jrow * sq2,
					c, s
				);
			}

			// -------------------------------------------------------
			// Step 2: Eliminate B(jrow, jrow-1) using a rotation
			// applied from the right.
			// Fortran: CTEMP = B(JROW, JROW)
			// -------------------------------------------------------
			idx = offsetB + jrow * sb1 + jrow * sb2;
			f[ 0 ] = B[ idx ];
			f[ 1 ] = B[ idx + 1 ];

			idx = offsetB + jrow * sb1 + ( jrow - 1 ) * sb2;
			g[ 0 ] = B[ idx ];
			g[ 1 ] = B[ idx + 1 ];

			zlartg( f, g, out );
			c = out[ 0 ];
			sr = out[ 1 ];
			si = out[ 2 ];

			// B(JROW, JROW) = R
			idx = offsetB + jrow * sb1 + jrow * sb2;
			B[ idx ] = out[ 3 ];
			B[ idx + 1 ] = out[ 4 ];

			// B(JROW, JROW-1) = 0
			idx = offsetB + jrow * sb1 + ( jrow - 1 ) * sb2;
			B[ idx ] = 0.0;
			B[ idx + 1 ] = 0.0;

			// Apply rotation to A from the right:
			// Fortran: CALL ZROT(IHI, A(1,JROW), 1, A(1,JROW-1), 1, C, S)
			// Iterates over rows (stride=1 complex element)
			s[ 0 ] = sr;
			s[ 1 ] = si;
			zrot(
				ihi,
				A, ra1, offsetA + jrow * sa2,
				A, ra1, offsetA + ( jrow - 1 ) * sa2,
				c, s
			);

			// Apply rotation to B from the right:
			// Fortran: CALL ZROT(JROW-1, B(1,JROW), 1, B(1,JROW-1), 1, C, S)
			// Iterates over rows (stride=1 complex element)
			zrot(
				jrow,
				B, rb1, offsetB + jrow * sb2,
				B, rb1, offsetB + ( jrow - 1 ) * sb2,
				c, s
			);

			// Apply rotation to Z if needed:
			// Fortran: CALL ZROT(N, Z(1,JROW), 1, Z(1,JROW-1), 1, C, S)
			// Iterates over rows (stride=1 complex element)
			if ( ilz ) {
				zrot(
					N,
					Z, rz1, offsetZ + jrow * sz2,
					Z, rz1, offsetZ + ( jrow - 1 ) * sz2,
					c, s
				);
			}
		}
	}

	return 0;
}


// EXPORTS //

module.exports = zgghrd;
