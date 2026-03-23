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
var Complex128Array = require( '@stdlib/array/complex128' );
var zsytf2 = require( '../../zsytf2/lib/base.js' );
var zlasyf = require( '../../zlasyf/lib/base.js' );
var NB = 32;
function zsytrf( uplo, N, A, strideA1, strideA2, offsetA, IPIV, strideIPIV, offsetIPIV ) {
	var ldwork, iinfo, info, result, nb, kb, W, k, j;
	info = 0;
	if ( N === 0 ) return 0;
	nb = NB;
	if ( nb > 1 && nb < N ) { ldwork = N; } else { nb = N; }
	if ( uplo === 'upper' ) {
		k = N;
		while ( k >= 1 ) {
			if ( k > nb ) {
				W = new Complex128Array( ldwork * nb );
				result = zlasyf( 'upper', k, nb, A, strideA1, strideA2, offsetA, IPIV, strideIPIV, offsetIPIV, W, 1, ldwork, 0 );
				kb = result.kb; iinfo = result.info;
			} else {
				iinfo = zsytf2( 'upper', k, A, strideA1, strideA2, offsetA, IPIV, strideIPIV, offsetIPIV );
				kb = k;
			}
			if ( info === 0 && iinfo > 0 ) info = iinfo;
			k -= kb;
		}
	} else {
		k = 0;
		while ( k < N ) {
			if ( k <= N - nb - 1 ) {
				W = new Complex128Array( ldwork * nb );
				result = zlasyf( 'lower', N - k, nb, A, strideA1, strideA2, offsetA + k * strideA1 + k * strideA2, IPIV, strideIPIV, offsetIPIV + k * strideIPIV, W, 1, ldwork, 0 );
				kb = result.kb; iinfo = result.info;
			} else {
				iinfo = zsytf2( 'lower', N - k, A, strideA1, strideA2, offsetA + k * strideA1 + k * strideA2, IPIV, strideIPIV, offsetIPIV + k * strideIPIV );
				kb = N - k;
			}
			if ( info === 0 && iinfo > 0 ) info = iinfo + k;
			for ( j = k; j < k + kb; j++ ) {
				if ( IPIV[ offsetIPIV + j * strideIPIV ] >= 0 ) { IPIV[ offsetIPIV + j * strideIPIV ] += k; }
				else { IPIV[ offsetIPIV + j * strideIPIV ] = ~( ( ~IPIV[ offsetIPIV + j * strideIPIV ] ) + k ); }
			}
			k += kb;
		}
	}
	return info;
}
module.exports = zsytrf;
