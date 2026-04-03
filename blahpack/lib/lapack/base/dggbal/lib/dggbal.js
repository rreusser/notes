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
var base = require( './base.js' );


// MAIN //

/**
* Balances a pair of general real matrices (A, B).
*
* @param {string} order - storage layout ('row-major' or 'column-major')
* @param {string} job - job type
* @param {NonNegativeInteger} N - order of matrices
* @param {Float64Array} A - first real matrix
* @param {PositiveInteger} LDA - leading dimension of A
* @param {Float64Array} B - second real matrix
* @param {PositiveInteger} LDB - leading dimension of B
* @param {Float64Array} LSCALE - left scaling factors
* @param {integer} strideLSCALE - stride for LSCALE
* @param {Float64Array} RSCALE - right scaling factors
* @param {integer} strideRSCALE - stride for RSCALE
* @param {Float64Array} WORK - workspace array
* @param {integer} strideWORK - stride for WORK
* @throws {TypeError} first argument must be a valid order
* @returns {Object} result with properties: info, ilo, ihi
*/
function dggbal( order, job, N, A, LDA, B, LDB, LSCALE, strideLSCALE, RSCALE, strideRSCALE, WORK, strideWORK ) {
	var sa1;
	var sa2;
	var sb1;
	var sb2;
	var ol;
	var or;
	var ow;

	if ( !isLayout( order ) ) {
		throw new TypeError( format( 'invalid argument. First argument must be a valid order. Value: `%s`.', order ) );
	}
	if ( order === 'column-major' ) {
		sa1 = 1;
		sa2 = LDA;
		sb1 = 1;
		sb2 = LDB;
	} else {
		sa1 = LDA;
		sa2 = 1;
		sb1 = LDB;
		sb2 = 1;
	}
	ol = stride2offset( N, strideLSCALE );
	or = stride2offset( N, strideRSCALE );
	ow = stride2offset( N, strideWORK );
	return base( job, N, A, sa1, sa2, 0, B, sb1, sb2, 0, LSCALE, strideLSCALE, ol, RSCALE, strideRSCALE, or, WORK, strideWORK, ow );
}


// EXPORTS //

module.exports = dggbal;
