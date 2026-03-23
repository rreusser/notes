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

/* eslint-disable max-len, max-params */

'use strict';


// MODULES //

var format = require( '@stdlib/string/format' );
var base = require( './base.js' );


// MAIN //

/**
* Compute the generalized eigenvalues and optionally the left and/or.
*
* @param {string} jobvl - 'N' for no left eigenvectors, 'V' for compute
* @param {string} jobvr - 'N' for no right eigenvectors, 'V' for compute
* @param {NonNegativeInteger} N - order of matrices A and B
* @param {Complex128Array} A - first complex matrix (modified in-place)
* @param {integer} strideA1 - stride of the first dimension of A (complex elements)
* @param {integer} strideA2 - stride of the second dimension of A (complex elements)
* @param {NonNegativeInteger} offsetA - starting index for A (complex elements)
* @param {Complex128Array} B - second complex matrix (modified in-place)
* @param {integer} strideB1 - stride of the first dimension of B (complex elements)
* @param {integer} strideB2 - stride of the second dimension of B (complex elements)
* @param {NonNegativeInteger} offsetB - starting index for B (complex elements)
* @param {Complex128Array} ALPHA - output eigenvalue numerators
* @param {integer} strideALPHA - stride for ALPHA (complex elements)
* @param {NonNegativeInteger} offsetALPHA - starting index for ALPHA (complex elements)
* @param {Complex128Array} BETA - output eigenvalue denominators
* @param {integer} strideBETA - stride for BETA (complex elements)
* @param {NonNegativeInteger} offsetBETA - starting index for BETA (complex elements)
* @param {Complex128Array} VL - left eigenvector matrix
* @param {integer} strideVL1 - stride of the first dimension of VL (complex elements)
* @param {integer} strideVL2 - stride of the second dimension of VL (complex elements)
* @param {NonNegativeInteger} offsetVL - starting index for VL (complex elements)
* @param {Complex128Array} VR - right eigenvector matrix
* @param {integer} strideVR1 - stride of the first dimension of VR (complex elements)
* @param {integer} strideVR2 - stride of the second dimension of VR (complex elements)
* @param {NonNegativeInteger} offsetVR - starting index for VR (complex elements)
* @throws {TypeError} first argument must be a valid job type
* @throws {TypeError} second argument must be a valid job type
* @throws {RangeError} third argument must be a nonnegative integer
* @returns {integer} INFO: 0=success, 1..N=QZ iteration failed to converge, N+1=other QZ failure, N+2=ZTGEVC error
*/
function zggev( jobvl, jobvr, N, A, strideA1, strideA2, offsetA, B, strideB1, strideB2, offsetB, ALPHA, strideALPHA, offsetALPHA, BETA, strideBETA, offsetBETA, VL, strideVL1, strideVL2, offsetVL, VR, strideVR1, strideVR2, offsetVR ) {
	if ( jobvl !== 'compute' && jobvl !== 'none' ) {
		throw new TypeError( format( 'invalid argument. First argument must be a valid job type. Value: `%s`.', jobvl ) );
	}
	if ( jobvr !== 'compute' && jobvr !== 'none' ) {
		throw new TypeError( format( 'invalid argument. Second argument must be a valid job type. Value: `%s`.', jobvr ) );
	}
	if ( N < 0 ) {
		throw new RangeError( format( 'invalid argument. Third argument must be a nonnegative integer. Value: `%d`.', N ) );
	}
	if ( N === 0 ) {
		return 0;
	}
	return base( jobvl, jobvr, N, A, strideA1, strideA2, offsetA, B, strideB1, strideB2, offsetB, ALPHA, strideALPHA, offsetALPHA, BETA, strideBETA, offsetBETA, VL, strideVL1, strideVL2, offsetVL, VR, strideVR1, strideVR2, offsetVR );
}


// EXPORTS //

module.exports = zggev;
