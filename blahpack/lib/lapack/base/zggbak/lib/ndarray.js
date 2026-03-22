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

var base = require( './base.js' );


// MAIN //

/**
* Back-transform eigenvectors of a balanced pair of matrices.
*
* @param {string} job - specifies the operation type
* @param {string} side - specifies the operation type
* @param {NonNegativeInteger} N - number of columns
* @param {integer} ilo - ilo
* @param {integer} ihi - ihi
* @param {Float64Array} LSCALE - input array
* @param {integer} strideLSCALE - stride length for `LSCALE`
* @param {NonNegativeInteger} offsetLSCALE - starting index for `LSCALE`
* @param {Float64Array} RSCALE - input array
* @param {integer} strideRSCALE - stride length for `RSCALE`
* @param {NonNegativeInteger} offsetRSCALE - starting index for `RSCALE`
* @param {NonNegativeInteger} M - number of rows
* @param {Float64Array} V - output matrix
* @param {integer} strideV1 - stride of the first dimension of `V`
* @param {integer} strideV2 - stride of the second dimension of `V`
* @param {NonNegativeInteger} offsetV - starting index for `V`
* @returns {integer} status code (0 = success)
*/
function zggbak( job, side, N, ilo, ihi, LSCALE, strideLSCALE, offsetLSCALE, RSCALE, strideRSCALE, offsetRSCALE, M, V, strideV1, strideV2, offsetV ) { // eslint-disable-line max-len, max-params
	return base( job, side, N, ilo, ihi, LSCALE, strideLSCALE, offsetLSCALE, RSCALE, strideRSCALE, offsetRSCALE, M, V, strideV1, strideV2, offsetV ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = zggbak;
