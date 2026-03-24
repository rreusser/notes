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
* Computes the singular value decomposition (SVD) of a complex M-by-N matrix A,.
*
* @param {string} jobu - 'A': all M columns of U are returned, 'S': first min(M,N) columns, 'O': overwrite A, 'N': no U
* @param {string} jobvt - 'A': all N rows of V^H are returned, 'S': first min(M,N) rows, 'O': overwrite A, 'N': no VT
* @param {NonNegativeInteger} M - number of rows of A
* @param {NonNegativeInteger} N - number of columns of A
* @param {Complex128Array} A - input/output matrix (column-major)
* @param {integer} strideA1 - stride of the first dimension of A (in complex elements)
* @param {integer} strideA2 - stride of the second dimension of A (in complex elements)
* @param {NonNegativeInteger} offsetA - starting index for A (in complex elements)
* @param {Float64Array} s - output array of real singular values (length min(M,N))
* @param {integer} strideS - stride for s
* @param {NonNegativeInteger} offsetS - starting index for s
* @param {Complex128Array} U - output matrix for left singular vectors
* @param {integer} strideU1 - stride of the first dimension of U (in complex elements)
* @param {integer} strideU2 - stride of the second dimension of U (in complex elements)
* @param {NonNegativeInteger} offsetU - starting index for U (in complex elements)
* @param {Complex128Array} VT - output matrix for right singular vectors
* @param {integer} strideVT1 - stride of the first dimension of VT (in complex elements)
* @param {integer} strideVT2 - stride of the second dimension of VT (in complex elements)
* @param {NonNegativeInteger} offsetVT - starting index for VT (in complex elements)
* @param {Complex128Array} WORK - complex workspace array
* @param {integer} strideWORK - stride for WORK (in complex elements)
* @param {NonNegativeInteger} offsetWORK - starting index for WORK (in complex elements)
* @param {integer} lwork - length of WORK array (in complex elements)
* @param {Float64Array} RWORK - real workspace array (length >= 5*min(M,N))
* @param {integer} strideRWORK - stride for RWORK
* @param {NonNegativeInteger} offsetRWORK - starting index for RWORK
* @throws {TypeError} first argument must be a valid job type
* @throws {TypeError} second argument must be a valid job type
* @throws {RangeError} third argument must be a nonnegative integer
* @throws {RangeError} fourth argument must be a nonnegative integer
* @returns {integer} info - 0 if successful, >0 if ZBDSQR did not converge
*/
function zgesvd( jobu, jobvt, M, N, A, strideA1, strideA2, offsetA, s, strideS, offsetS, U, strideU1, strideU2, offsetU, VT, strideVT1, strideVT2, offsetVT, WORK, strideWORK, offsetWORK, lwork, RWORK, strideRWORK, offsetRWORK ) {
	if ( jobu !== 'all' && jobu !== 'some' && jobu !== 'overwrite' && jobu !== 'none' ) {
		throw new TypeError( format( 'invalid argument. First argument must be a valid job type. Value: `%s`.', jobu ) );
	}
	if ( jobvt !== 'all' && jobvt !== 'some' && jobvt !== 'overwrite' && jobvt !== 'none' ) {
		throw new TypeError( format( 'invalid argument. Second argument must be a valid job type. Value: `%s`.', jobvt ) );
	}
	if ( M < 0 ) {
		throw new RangeError( format( 'invalid argument. Third argument must be a nonnegative integer. Value: `%d`.', M ) );
	}
	if ( N < 0 ) {
		throw new RangeError( format( 'invalid argument. Fourth argument must be a nonnegative integer. Value: `%d`.', N ) );
	}
	if ( M === 0 || N === 0 ) {
		return 0;
	}
	var JOBU_MAP = { 'all': 'all-columns', 'some': 'economy', 'overwrite': 'overwrite', 'none': 'none' };
	var JOBVT_MAP = { 'all': 'all-rows', 'some': 'economy', 'overwrite': 'overwrite', 'none': 'none' };
	return base( JOBU_MAP[ jobu ], JOBVT_MAP[ jobvt ], M, N, A, strideA1, strideA2, offsetA, s, strideS, offsetS, U, strideU1, strideU2, offsetU, VT, strideVT1, strideVT2, offsetVT, WORK, strideWORK, offsetWORK, lwork, RWORK, strideRWORK, offsetRWORK );
}


// EXPORTS //

module.exports = zgesvd;
