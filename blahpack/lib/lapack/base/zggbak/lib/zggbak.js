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
* Back-transform eigenvectors of a balanced pair of general real matrices.
*
* @param {string} order - storage layout ('row-major' or 'column-major')
* @param {string} job - TODO
* @param {string} side - TODO
* @param {NonNegativeInteger} N - TODO
* @param {integer} ilo - TODO
* @param {integer} ihi - TODO
* @param {Float64Array} LSCALE - input array
* @param {integer} strideLSCALE - `LSCALE` stride length
* @param {Float64Array} RSCALE - input array
* @param {integer} strideRSCALE - `RSCALE` stride length
* @param {NonNegativeInteger} M - TODO
* @param {Complex128Array} V - input matrix
* @param {PositiveInteger} LDV - leading dimension of `V`
* @throws {TypeError} first argument must be a valid order
* @returns {*} result
*/
function zggbak( order, job, side, N, ilo, ihi, LSCALE, strideLSCALE, RSCALE, strideRSCALE, M, V, LDV ) {
	var sv1;
	var sv2;
	var ol;
	var or;

	if ( !isLayout( order ) ) {
		throw new TypeError( format( 'invalid argument. First argument must be a valid order. Value: `%s`.', order ) );
	}
	if ( order === 'column-major' ) {
		sv1 = 1;
		sv2 = LDV;
	} else {
		sv1 = LDV;
		sv2 = 1;
	}
	ol = stride2offset( N, strideLSCALE );
	or = stride2offset( N, strideRSCALE );
	return base( job, side, N, ilo, ihi, LSCALE, strideLSCALE, ol, RSCALE, strideRSCALE, or, M, V, sv1, sv2, 0 );
}


// EXPORTS //

module.exports = zggbak;
