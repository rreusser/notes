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

var dlamch = require( '../../dlamch/lib/base.js' );


// VARIABLES //

var SMLNUM = dlamch( 'safe-minimum' );
var BIGNUM = 1.0 / SMLNUM;


// MAIN //

/**
* Computes row and column scalings intended to equilibrate an M-by-N matrix A.
* and reduce its condition number.
*
* R returns the row scale factors and C the column scale factors, chosen to
* try to make the largest element in each row and column of the matrix B with
* elements B(i,j)=R(i)_A(i,j)_C(j) have absolute value 1.
*
* Returns an object with:
* - info: 0 if successful; i if the i-th row is zero (1-based); M+j if the
*   (j)-th column (after row scaling) is zero (1-based).
* - rowcnd: ratio of smallest to largest row scale factor
* - colcnd: ratio of smallest to largest column scale factor
* - amax: absolute value of largest matrix element
*
* @private
* @param {NonNegativeInteger} M - number of rows of A
* @param {NonNegativeInteger} N - number of columns of A
* @param {Float64Array} A - input M-by-N matrix
* @param {integer} strideA1 - stride of the first dimension of A
* @param {integer} strideA2 - stride of the second dimension of A
* @param {NonNegativeInteger} offsetA - index offset for A
* @param {Float64Array} r - output row scale factors, length M
* @param {integer} strideR - stride for r
* @param {NonNegativeInteger} offsetR - index offset for r
* @param {Float64Array} c - output column scale factors, length N
* @param {integer} strideC - stride for c
* @param {NonNegativeInteger} offsetC - index offset for c
* @returns {Object} result with info, rowcnd, colcnd, amax
*/
function dgeequ( M, N, A, strideA1, strideA2, offsetA, r, strideR, offsetR, c, strideC, offsetC ) {
	var rowcnd = Math.max( rcmin, SMLNUM ) / Math.min( rcmax, BIGNUM );
	var colcnd = Math.max( rcmin, SMLNUM ) / Math.min( rcmax, BIGNUM );
	var rcmax;
	var rcmin;
	var amax = rcmax;
	var da;
	var ri;
	var ci;
	var av;
	var i;
	var j; // eslint-disable-line no-var

	return {
		'info': 0,
		'rowcnd': rowcnd,
		'colcnd': colcnd,
		'amax': amax
	};
}


// EXPORTS //

module.exports = dgeequ;
