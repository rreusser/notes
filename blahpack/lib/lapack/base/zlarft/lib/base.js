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

var zgemv = require( '../../../../blas/base/zgemv/lib/base.js' );
var zgemm = require( '../../../../blas/base/zgemm/lib/base.js' );
var ztrmv = require( '../../../../blas/base/ztrmv/lib/base.js' );

// VARIABLES //

var ONE = new Float64Array( [ 1.0, 0.0 ] );

// MAIN //

/**
* Form the triangular factor T of a complex block reflector H of order N,
* which is defined as a product of K elementary reflectors.
*
* If DIRECT = 'F', H = H(1) H(2) ... H(k) and T is upper triangular.
* If DIRECT = 'B', H = H(k) ... H(2) H(1) and T is lower triangular.
*
* Complex elements are stored as interleaved real/imaginary pairs.
* Strides are in complex-element units.
*
* Supports both STOREV='C' (columnwise) and STOREV='R' (rowwise).
*
* @private
* @param {string} direct - 'F' for forward, 'B' for backward
* @param {string} storev - 'C' for columnwise, 'R' for rowwise
* @param {NonNegativeInteger} N - order of the block reflector
* @param {NonNegativeInteger} K - number of elementary reflectors
* @param {Float64Array} V - matrix of reflector vectors (interleaved complex)
* @param {integer} strideV1 - stride of first dim of V (complex elements)
* @param {integer} strideV2 - stride of second dim of V (complex elements)
* @param {NonNegativeInteger} offsetV - starting index for V
* @param {Float64Array} TAU - array of scalar factors (interleaved complex)
* @param {integer} strideTAU - stride for TAU (in complex elements)
* @param {NonNegativeInteger} offsetTAU - starting index for TAU
* @param {Float64Array} T - output triangular matrix (interleaved complex)
* @param {integer} strideT1 - stride of first dim of T (complex elements)
* @param {integer} strideT2 - stride of second dim of T (complex elements)
* @param {NonNegativeInteger} offsetT - starting index for T
*/
function zlarft( direct, storev, N, K, V, strideV1, strideV2, offsetV, TAU, strideTAU, offsetTAU, T, strideT1, strideT2, offsetT ) { // eslint-disable-line max-len, max-params
	var prevlastv;
	var negTauI;
	var negTauR;
	var lastv;
	var tauR;
	var tauI;
	var vr;
	var vi;
	var sv1;
	var sv2;
	var st1;
	var st2;
	var stau;
	var it;
	var iv;
	var jt;
	var i;
	var j;
	var jj;

	if ( N === 0 ) {
		return;
	}

	// Convert strides to double units
	sv1 = strideV1 * 2;
	sv2 = strideV2 * 2;
	st1 = strideT1 * 2;
	st2 = strideT2 * 2;
	stau = strideTAU * 2;

	if ( ( direct === 'F' || direct === 'f' ) ) {
		prevlastv = N;
		for ( i = 0; i < K; i++ ) {
			prevlastv = Math.max( prevlastv, i );
			tauR = TAU[ offsetTAU + i * stau ];
			tauI = TAU[ offsetTAU + i * stau + 1 ];

			if ( tauR === 0.0 && tauI === 0.0 ) {
				// H(i) = I: set T(:,i) column to zero
				for ( j = 0; j <= i; j++ ) {
					it = offsetT + j * st1 + i * st2;
					T[ it ] = 0.0;
					T[ it + 1 ] = 0.0;
				}
			} else {
				// General case for columnwise storage
				if ( storev === 'C' || storev === 'c' ) {
					// Skip trailing zeros in V(:,i)
					lastv = N;
					for ( jj = N - 1; jj > i; jj-- ) {
						iv = offsetV + jj * sv1 + i * sv2;
						if ( V[ iv ] !== 0.0 || V[ iv + 1 ] !== 0.0 ) {
							break;
						}
						lastv = jj;
					}

					// T(0:i-1, i) = -tau(i) * conj(V(i, 0:i-1))
					negTauR = -tauR;
					negTauI = -tauI;
					for ( j = 0; j < i; j++ ) {
						iv = offsetV + i * sv1 + j * sv2;
						vr = V[ iv ];
						vi = -V[ iv + 1 ]; // conjugate
						// -tau * conj(V(i,j))
						it = offsetT + j * st1 + i * st2;
						T[ it ] = negTauR * vr - negTauI * vi;
						T[ it + 1 ] = negTauR * vi + negTauI * vr;
					}
					jj = Math.min( lastv, prevlastv );

					// T(0:i-1, i) += -tau(i) * V(i+1:jj-1, 0:i-1)^H * V(i+1:jj-1, i)
					if ( jj - i - 1 > 0 ) {
						var negTau = new Float64Array( [ negTauR, negTauI ] );
						zgemv( 'C', jj - i - 1, i, negTau,
							V, strideV1, strideV2, offsetV + ( i + 1 ) * sv1,
							V, strideV1, offsetV + ( i + 1 ) * sv1 + i * sv2,
							ONE,
							T, strideT1, offsetT + i * st2 );
					}
				} else {
					// Row-wise storage: V(i,:) holds reflector i
					// Skip trailing zeros in V(i,:)
					lastv = N;
					for ( jj = N - 1; jj > i; jj-- ) {
						iv = offsetV + i * sv1 + jj * sv2;
						if ( V[ iv ] !== 0.0 || V[ iv + 1 ] !== 0.0 ) {
							break;
						}
						lastv = jj;
					}

					// T(0:i-1, i) = -tau(i) * V(0:i-1, i)  (no conjugate for rowwise)
					negTauR = -tauR;
					negTauI = -tauI;
					for ( j = 0; j < i; j++ ) {
						iv = offsetV + j * sv1 + i * sv2;
						vr = V[ iv ];
						vi = V[ iv + 1 ];
						it = offsetT + j * st1 + i * st2;
						T[ it ] = negTauR * vr - negTauI * vi;
						T[ it + 1 ] = negTauR * vi + negTauI * vr;
					}
					jj = Math.min( lastv, prevlastv );

					// T(0:i-1, i) += -tau(i) * V(0:i-1, i+1:jj-1) * V(i, i+1:jj-1)^H
					if ( jj - i - 1 > 0 ) {
						var negTauR2 = new Float64Array( [ negTauR, negTauI ] ); // eslint-disable-line no-var
						zgemm( 'N', 'C', i, 1, jj - i - 1, negTauR2,
							V, strideV1, strideV2, offsetV + ( i + 1 ) * sv2,
							V, strideV1, strideV2, offsetV + i * sv1 + ( i + 1 ) * sv2,
							ONE, T, strideT1, strideT2, offsetT + i * st2 );
					}
				}

				// T(0:i-1, i) := T(0:i-1, 0:i-1) * T(0:i-1, i)
				if ( i > 0 ) {
					ztrmv( 'U', 'N', 'N', i, T, strideT1, strideT2, offsetT,
						T, strideT1, offsetT + i * st2 );
				}
				// T(i, i) = tau(i)
				it = offsetT + i * st1 + i * st2;
				T[ it ] = tauR;
				T[ it + 1 ] = tauI;

				if ( i > 0 ) {
					prevlastv = Math.max( prevlastv, lastv );
				} else {
					prevlastv = lastv;
				}
			}
		}
	} else {
		// Backward: T is lower triangular
		prevlastv = 0;
		for ( i = K - 1; i >= 0; i-- ) {
			tauR = TAU[ offsetTAU + i * stau ];
			tauI = TAU[ offsetTAU + i * stau + 1 ];

			if ( tauR === 0.0 && tauI === 0.0 ) {
				// H(i) = I
				for ( j = i; j < K; j++ ) {
					it = offsetT + j * st1 + i * st2;
					T[ it ] = 0.0;
					T[ it + 1 ] = 0.0;
				}
			} else {
				if ( i < K - 1 ) {
					if ( storev === 'C' || storev === 'c' ) {
						// Skip leading zeros in V(:,i)
						lastv = 0;
						for ( jj = 0; jj < i; jj++ ) {
							iv = offsetV + jj * sv1 + i * sv2;
							if ( V[ iv ] !== 0.0 || V[ iv + 1 ] !== 0.0 ) {
								break;
							}
							lastv = jj + 1;
						}

						// T(i+1:K-1, i) = -tau(i) * conj(V(N-K+i, i+1:K-1))
						negTauR = -tauR;
						negTauI = -tauI;
						for ( j = i + 1; j < K; j++ ) {
							iv = offsetV + ( N - K + i ) * sv1 + j * sv2;
							vr = V[ iv ];
							vi = -V[ iv + 1 ]; // conjugate
							it = offsetT + j * st1 + i * st2;
							T[ it ] = negTauR * vr - negTauI * vi;
							T[ it + 1 ] = negTauR * vi + negTauI * vr;
						}
						jj = Math.max( lastv, prevlastv );

						// T(i+1:K-1, i) += -tau(i) * V(jj:N-K+i-1, i+1:K-1)^H * V(jj:N-K+i-1, i)
						if ( N - K + i - jj > 0 ) {
							var negTauB = new Float64Array( [ negTauR, negTauI ] );
							zgemv( 'C', N - K + i - jj, K - i - 1, negTauB,
								V, strideV1, strideV2, offsetV + jj * sv1 + ( i + 1 ) * sv2,
								V, strideV1, offsetV + jj * sv1 + i * sv2,
								ONE,
								T, strideT1, offsetT + ( i + 1 ) * st1 + i * st2 );
						}
					} else {
						// Row-wise storage: V(i,:) holds reflector i
						// Skip leading zeros in V(i,:)
						lastv = 0;
						for ( jj = 0; jj < i; jj++ ) {
							iv = offsetV + i * sv1 + jj * sv2;
							if ( V[ iv ] !== 0.0 || V[ iv + 1 ] !== 0.0 ) {
								break;
							}
							lastv = jj + 1;
						}

						// T(i+1:K-1, i) = -tau(i) * V(i+1:K-1, N-K+i) (no conjugate)
						negTauR = -tauR;
						negTauI = -tauI;
						for ( j = i + 1; j < K; j++ ) {
							iv = offsetV + j * sv1 + ( N - K + i ) * sv2;
							vr = V[ iv ];
							vi = V[ iv + 1 ];
							it = offsetT + j * st1 + i * st2;
							T[ it ] = negTauR * vr - negTauI * vi;
							T[ it + 1 ] = negTauR * vi + negTauI * vr;
						}
						jj = Math.max( lastv, prevlastv );

						// T(i+1:K-1, i) += -tau(i) * V(i+1:K-1, jj:N-K+i-1) * V(i, jj:N-K+i-1)^H
						if ( N - K + i - jj > 0 ) {
							var negTauB2 = new Float64Array( [ negTauR, negTauI ] ); // eslint-disable-line no-var
							zgemm( 'N', 'C', K - i - 1, 1, N - K + i - jj, negTauB2,
								V, strideV1, strideV2, offsetV + ( i + 1 ) * sv1 + jj * sv2,
								V, strideV1, strideV2, offsetV + i * sv1 + jj * sv2,
								ONE, T, strideT1, strideT2, offsetT + ( i + 1 ) * st1 + i * st2 );
						}
					}

					// T(i+1:K-1, i) := T(i+1:K-1, i+1:K-1) * T(i+1:K-1, i)
					ztrmv( 'L', 'N', 'N', K - i - 1, T, strideT1, strideT2,
						offsetT + ( i + 1 ) * st1 + ( i + 1 ) * st2,
						T, strideT1, offsetT + ( i + 1 ) * st1 + i * st2 );
					if ( i > 0 ) {
						prevlastv = Math.min( prevlastv, lastv );
					} else {
						prevlastv = lastv;
					}
				}
				it = offsetT + i * st1 + i * st2;
				T[ it ] = tauR;
				T[ it + 1 ] = tauI;
			}
		}
	}
}


// EXPORTS //

module.exports = zlarft;
