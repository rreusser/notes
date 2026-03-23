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
* Computes all eigenvalues and, optionally, eigenvectors of a real symmetric.
*
* @param {string} compz - specifies whether eigenvectors are computed
* @param {NonNegativeInteger} N - order of the matrix
* @param {Float64Array} d - diagonal elements of the tridiagonal matrix (length N)
* @param {integer} strideD - stride length for `d`
* @param {NonNegativeInteger} offsetD - starting index for `d`
* @param {Float64Array} e - subdiagonal elements of the tridiagonal matrix (length N-1)
* @param {integer} strideE - stride length for `e`
* @param {NonNegativeInteger} offsetE - starting index for `e`
* @param {Complex128Array} Z - unitary matrix Z (N-by-N)
* @param {integer} strideZ1 - stride of the first dimension of `Z` (in complex elements)
* @param {integer} strideZ2 - stride of the second dimension of `Z` (in complex elements)
* @param {NonNegativeInteger} offsetZ - starting index for `Z` (in complex elements)
* @param {Float64Array} WORK - workspace array (length >= 2*(N-1))
* @param {integer} strideWORK - stride length for `WORK`
* @param {NonNegativeInteger} offsetWORK - starting index for `WORK`
* @throws {TypeError} first argument must be a valid computation flag
* @throws {RangeError} second argument must be a nonnegative integer
* @returns {integer} INFO - 0 if successful, >0 if INFO eigenvalues did not converge
*/
function zsteqr( compz, N, d, strideD, offsetD, e, strideE, offsetE, Z, strideZ1, strideZ2, offsetZ, WORK, strideWORK, offsetWORK ) {
	if ( compz !== 'none' && compz !== 'initialize' && compz !== 'compute' ) {
		throw new TypeError( format( 'invalid argument. First argument must be a valid computation flag. Value: `%s`.', compz ) );
	}
	if ( N < 0 ) {
		throw new RangeError( format( 'invalid argument. Second argument must be a nonnegative integer. Value: `%d`.', N ) );
	}
	if ( N === 0 ) {
		return 0;
	}
	return base( compz, N, d, strideD, offsetD, e, strideE, offsetE, Z, strideZ1, strideZ2, offsetZ, WORK, strideWORK, offsetWORK );
}


// EXPORTS //

module.exports = zsteqr;
