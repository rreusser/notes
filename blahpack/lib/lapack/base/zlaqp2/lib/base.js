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

var zlarfg = require( '../../zlarfg/lib/base.js' );
var zlarf = require( '../../zlarf/lib/base.js' );
var zswap = require( '../../../../blas/base/zswap/lib/base.js' );
var idamax = require( '../../../../blas/base/idamax/lib/base.js' );
var dznrm2 = require( '../../../../blas/base/dznrm2/lib/base.js' );
var dlamch = require( '../../dlamch/lib/base.js' );
var cmplx = require( '../../../../cmplx.js' );


// MAIN //

/**
* Computes a QR factorization with column pivoting of the M-by-N matrix A
* using an unblocked algorithm. The factored form is A*P = Q*R.
*
* This is an internal routine called by ZGEQP3. It factors the submatrix
* A(OFFSET+1:M, 1:N) with MN = min(M-OFFSET, N) reflectors.
*
* On entry, JPVT(I) = I (1-based column identity in the Fortran convention).
* On exit, JPVT records the column permutation.
*
* VN1(J) and VN2(J) are the partial and original column norms of the
* unfactored submatrix A(OFFSET+1:M, J). On exit they are updated.
*
* Complex elements are stored as interleaved real/imaginary pairs.
* Strides are in complex-element units.
*
* @private
* @param {NonNegativeInteger} M - total number of rows of A
* @param {NonNegativeInteger} N - number of columns of A
* @param {NonNegativeInteger} offset - number of rows already factored
* @param {Float64Array} A - input/output matrix (interleaved complex, column-major)
* @param {integer} strideA1 - stride of the first dimension of A (complex elements)
* @param {integer} strideA2 - stride of the second dimension of A (complex elements)
* @param {NonNegativeInteger} offsetA - starting Float64 index for A
* @param {Int32Array} JPVT - column permutation array (length >= N)
* @param {integer} strideJPVT - stride for JPVT
* @param {NonNegativeInteger} offsetJPVT - starting index for JPVT
* @param {Float64Array} TAU - output reflector scalars (interleaved complex)
* @param {integer} strideTAU - stride for TAU (complex elements)
* @param {NonNegativeInteger} offsetTAU - starting Float64 index for TAU
* @param {Float64Array} VN1 - partial column norms (real array)
* @param {integer} strideVN1 - stride for VN1
* @param {NonNegativeInteger} offsetVN1 - starting index for VN1
* @param {Float64Array} VN2 - original column norms (real array)
* @param {integer} strideVN2 - stride for VN2
* @param {NonNegativeInteger} offsetVN2 - starting index for VN2
* @param {Float64Array} WORK - workspace (interleaved complex)
* @param {integer} strideWORK - stride for WORK (complex elements)
* @param {NonNegativeInteger} offsetWORK - starting Float64 index for WORK
*/
function zlaqp2( M, N, offset, A, strideA1, strideA2, offsetA, JPVT, strideJPVT, offsetJPVT, TAU, strideTAU, offsetTAU, VN1, strideVN1, offsetVN1, VN2, strideVN2, offsetVN2, WORK, strideWORK, offsetWORK ) { // eslint-disable-line max-len, max-params
	var conjTau;
	var tol3z;
	var offpi;
	var itemp;
	var temp2;
	var temp;
	var aii;
	var pvt;
	var sa1;
	var sa2;
	var oA;
	var mn;
	var i;
	var j;

	sa1 = strideA1;
	sa2 = strideA2;
	oA = offsetA;

	mn = Math.min( M - offset, N );
	tol3z = Math.sqrt( dlamch( 'Epsilon' ) );

	conjTau = new Float64Array( 2 );
	aii = new Float64Array( 2 );

	for ( i = 0; i < mn; i++ ) {
		offpi = offset + i;

		// Determine pivot: find column with largest norm in VN1(i:N-1)
		pvt = i + idamax( N - i, VN1, strideVN1, offsetVN1 + i * strideVN1 );

		// Swap columns if needed
		if ( pvt !== i ) {
			// Swap columns pvt and i of A (all M rows)
			zswap( M, A, sa1, oA + 2 * pvt * sa2, A, sa1, oA + 2 * i * sa2 );

			// Swap JPVT entries
			itemp = JPVT[ offsetJPVT + pvt * strideJPVT ];
			JPVT[ offsetJPVT + pvt * strideJPVT ] = JPVT[ offsetJPVT + i * strideJPVT ];
			JPVT[ offsetJPVT + i * strideJPVT ] = itemp;

			// Swap norms
			VN1[ offsetVN1 + pvt * strideVN1 ] = VN1[ offsetVN1 + i * strideVN1 ];
			VN2[ offsetVN2 + pvt * strideVN2 ] = VN2[ offsetVN2 + i * strideVN2 ];
		}

		// Generate elementary reflector H(i) to annul A(offpi+1:M-1, i)
		if ( offpi < M - 1 ) {
			// zlarfg(N, alpha, offsetAlpha, x, strideX, offsetX, tau, offsetTau)
			zlarfg(
				M - offpi,
				A, oA + 2 * ( offpi * sa1 + i * sa2 ),
				A, sa1, oA + 2 * ( ( offpi + 1 ) * sa1 + i * sa2 ),
				TAU, offsetTAU + 2 * i * strideTAU
			);
		} else {
			zlarfg(
				1,
				A, oA + 2 * ( ( M - 1 ) * sa1 + i * sa2 ),
				A, sa1, oA + 2 * ( ( M - 1 ) * sa1 + i * sa2 ),
				TAU, offsetTAU + 2 * i * strideTAU
			);
		}

		if ( i < N - 1 ) {
			// Apply H(i)^H to A(offpi:M-1, i+1:N-1) from the left
			// Save A(offpi, i) and set it to 1
			aii[ 0 ] = A[ oA + 2 * ( offpi * sa1 + i * sa2 ) ];
			aii[ 1 ] = A[ oA + 2 * ( offpi * sa1 + i * sa2 ) + 1 ];
			A[ oA + 2 * ( offpi * sa1 + i * sa2 ) ] = 1.0;
			A[ oA + 2 * ( offpi * sa1 + i * sa2 ) + 1 ] = 0.0;

			// conj(tau(i))
			conjTau[ 0 ] = TAU[ offsetTAU + 2 * i * strideTAU ];
			conjTau[ 1 ] = -TAU[ offsetTAU + 2 * i * strideTAU + 1 ];

			zlarf(
				'L', M - offpi, N - i - 1,
				A, sa1, oA + 2 * ( offpi * sa1 + i * sa2 ),
				conjTau, 0,
				A, sa1, sa2, oA + 2 * ( offpi * sa1 + ( i + 1 ) * sa2 ),
				WORK, strideWORK, offsetWORK
			);

			// Restore A(offpi, i)
			A[ oA + 2 * ( offpi * sa1 + i * sa2 ) ] = aii[ 0 ];
			A[ oA + 2 * ( offpi * sa1 + i * sa2 ) + 1 ] = aii[ 1 ];
		}

		// Update partial column norms
		for ( j = i + 1; j < N; j++ ) {
			if ( VN1[ offsetVN1 + j * strideVN1 ] !== 0.0 ) {
				// temp = |A(offpi, j)| / VN1(j)
				temp = cmplx.abs(
					A.subarray(
						oA + 2 * ( offpi * sa1 + j * sa2 ),
						oA + 2 * ( offpi * sa1 + j * sa2 ) + 2
					)
				) / VN1[ offsetVN1 + j * strideVN1 ];
				temp = Math.max( 0.0, ( 1.0 - temp * temp ) );
				temp2 = temp * Math.pow(
					VN1[ offsetVN1 + j * strideVN1 ] /
					VN2[ offsetVN2 + j * strideVN2 ], 2
				);

				if ( temp2 <= tol3z ) {
					if ( offpi < M - 1 ) {
						VN1[ offsetVN1 + j * strideVN1 ] = dznrm2(
							M - offpi - 1,
							A, sa1, oA + 2 * ( ( offpi + 1 ) * sa1 + j * sa2 )
						);
						VN2[ offsetVN2 + j * strideVN2 ] = VN1[ offsetVN1 + j * strideVN1 ];
					} else {
						VN1[ offsetVN1 + j * strideVN1 ] = 0.0;
						VN2[ offsetVN2 + j * strideVN2 ] = 0.0;
					}
				} else {
					VN1[ offsetVN1 + j * strideVN1 ] *= Math.sqrt( temp );
				}
			}
		}
	}
}


// EXPORTS //

module.exports = zlaqp2;
