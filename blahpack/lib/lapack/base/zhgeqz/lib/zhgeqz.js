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
* Compute the eigenvalues of a complex matrix pair (H, T), where H is.
*
* @param {string} order - storage layout ('row-major' or 'column-major')
* @param {string} job - TODO
* @param {string} compq - TODO
* @param {string} compz - TODO
* @param {NonNegativeInteger} N - TODO
* @param {integer} ilo - TODO
* @param {integer} ihi - TODO
* @param {Complex128Array} H - input matrix
* @param {PositiveInteger} LDH - leading dimension of `H`
* @param {Complex128Array} T - input matrix
* @param {PositiveInteger} LDT - leading dimension of `T`
* @param {Complex128Array} ALPHA - input array
* @param {integer} strideALPHA - `ALPHA` stride length
* @param {Complex128Array} BETA - input array
* @param {integer} strideBETA - `BETA` stride length
* @param {Complex128Array} Q - input matrix
* @param {PositiveInteger} LDQ - leading dimension of `Q`
* @param {Complex128Array} Z - input matrix
* @param {PositiveInteger} LDZ - leading dimension of `Z`
* @param {Complex128Array} WORK - input array
* @param {integer} strideWORK - `WORK` stride length
* @param {integer} lwork - TODO
* @param {Float64Array} RWORK - input array
* @param {integer} strideRWORK - `RWORK` stride length
* @throws {TypeError} first argument must be a valid order
* @returns {*} result
*/
function zhgeqz( order, job, compq, compz, N, ilo, ihi, H, LDH, T, LDT, ALPHA, strideALPHA, BETA, strideBETA, Q, LDQ, Z, LDZ, WORK, strideWORK, lwork, RWORK, strideRWORK ) {
	var sh1;
	var sh2;
	var st1;
	var st2;
	var sq1;
	var sq2;
	var sz1;
	var sz2;
	var oa;
	var ob;
	var ow;
	var or;

	if ( !isLayout( order ) ) {
		throw new TypeError( format( 'invalid argument. First argument must be a valid order. Value: `%s`.', order ) );
	}
	if ( order === 'column-major' ) {
		sh1 = 1;
		sh2 = LDH;
		st1 = 1;
		st2 = LDT;
		sq1 = 1;
		sq2 = LDQ;
		sz1 = 1;
		sz2 = LDZ;
	} else {
		sh1 = LDH;
		sh2 = 1;
		st1 = LDT;
		st2 = 1;
		sq1 = LDQ;
		sq2 = 1;
		sz1 = LDZ;
		sz2 = 1;
	}
	oa = stride2offset( N, strideALPHA );
	ob = stride2offset( N, strideBETA );
	ow = stride2offset( N, strideWORK );
	or = stride2offset( N, strideRWORK );
	return base( job, compq, compz, N, ilo, ihi, H, sh1, sh2, 0, T, st1, st2, 0, ALPHA, strideALPHA, oa, BETA, strideBETA, ob, Q, sq1, sq2, 0, Z, sz1, sz2, 0, WORK, strideWORK, ow, lwork, RWORK, strideRWORK, or );
}


// EXPORTS //

module.exports = zhgeqz;
