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

var isLayout = require( '@stdlib/blas/base/assert/is-layout' );
var stride2offset = require( '@stdlib/strided/base/stride2offset' );
var format = require( '@stdlib/string/format' );
var max = require( '@stdlib/math/base/special/fast/max' );
var base = require( './base.js' );


// MAIN //

/**
* Computes the singular value decomposition (SVD) of a real M-by-N matrix A,.
*
* @param {string} order - storage layout ('row-major' or 'column-major')
* @param {string} jobu - `'all'`: all M columns of U returned, `'some'`: first min(M,N) columns, `'overwrite'`: overwrite A, `'none'`: no U
* @param {string} jobvt - `'all'`: all N rows of V^T returned, `'some'`: first min(M,N) rows, `'overwrite'`: overwrite A, `'none'`: no VT
* @param {NonNegativeInteger} M - number of rows of A
* @param {NonNegativeInteger} N - number of columns of A
* @param {Float64Array} A - input matrix
* @param {PositiveInteger} LDA - leading dimension of `A`
* @param {Float64Array} s - input array
* @param {integer} strideS - `s` stride length
* @param {Float64Array} U - input matrix
* @param {PositiveInteger} LDU - leading dimension of `U`
* @param {Float64Array} VT - input matrix
* @param {PositiveInteger} LDVT - leading dimension of `VT`
* @throws {TypeError} first argument must be a valid order
* @returns {*} result
*/
function dgesvd( order, jobu, jobvt, M, N, A, LDA, s, strideS, U, LDU, VT, LDVT ) {
	var sa1;
	var sa2;
	var su1;
	var su2;
	var sv1;
	var sv2;
	var os;

	if ( !isLayout( order ) ) {
		throw new TypeError( format( 'invalid argument. First argument must be a valid order. Value: `%s`.', order ) );
	}
	if ( M < 0 ) {
		throw new RangeError( format( 'invalid argument. Fourth argument must be a nonnegative integer. Value: `%d`.', M ) );
	}
	if ( N < 0 ) {
		throw new RangeError( format( 'invalid argument. Fifth argument must be a nonnegative integer. Value: `%d`.', N ) );
	}
	if ( order === 'row-major' && LDVT < max( 1, N ) ) {
		throw new RangeError( format( 'invalid argument. Thirteenth argument must be greater than or equal to max(1,N). Value: `%d`.', LDVT ) );
	}
	if ( order === 'column-major' && LDVT < max( 1, M ) ) {
		throw new RangeError( format( 'invalid argument. Thirteenth argument must be greater than or equal to max(1,M). Value: `%d`.', LDVT ) );
	}
	if ( order === 'row-major' && LDU < max( 1, N ) ) {
		throw new RangeError( format( 'invalid argument. Eleventh argument must be greater than or equal to max(1,N). Value: `%d`.', LDU ) );
	}
	if ( order === 'column-major' && LDU < max( 1, M ) ) {
		throw new RangeError( format( 'invalid argument. Eleventh argument must be greater than or equal to max(1,M). Value: `%d`.', LDU ) );
	}
	if ( order === 'row-major' && LDA < max( 1, N ) ) {
		throw new RangeError( format( 'invalid argument. Seventh argument must be greater than or equal to max(1,N). Value: `%d`.', LDA ) );
	}
	if ( order === 'column-major' && LDA < max( 1, M ) ) {
		throw new RangeError( format( 'invalid argument. Seventh argument must be greater than or equal to max(1,M). Value: `%d`.', LDA ) );
	}
	if ( jobu !== 'all-columns' && jobu !== 'economy' && jobu !== 'overwrite' && jobu !== 'none' ) {
		throw new TypeError( format( 'invalid argument. Second argument must be a valid `jobu` value. Value: `%s`.', jobu ) );
	}
	if ( jobvt !== 'all-rows' && jobvt !== 'economy' && jobvt !== 'overwrite' && jobvt !== 'none' ) {
		throw new TypeError( format( 'invalid argument. Third argument must be a valid `jobvt` value. Value: `%s`.', jobvt ) );
	}
	if ( order === 'column-major' ) {
		sa1 = 1;
		sa2 = LDA;
		su1 = 1;
		su2 = LDU;
		sv1 = 1;
		sv2 = LDVT;
	} else {
		sa1 = LDA;
		sa2 = 1;
		su1 = LDU;
		su2 = 1;
		sv1 = LDVT;
		sv2 = 1;
	}
	os = stride2offset( N, strideS );
	return base( jobu, jobvt, M, N, A, sa1, sa2, 0, s, strideS, os, U, su1, su2, 0, VT, sv1, sv2, 0 );
}


// EXPORTS //

module.exports = dgesvd;
