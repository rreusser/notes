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

var float64view = require( '../../../../float64view.js' );

// MAIN //

/**
* Conjugate a complex vector in-place.
*
* @private
* @param {NonNegativeInteger} N - number of complex elements
* @param {(Complex128Array|Float64Array)} x - complex input vector
* @param {integer} stride - stride for `x` (in complex elements)
* @param {NonNegativeInteger} offset - starting index for `x` (in complex elements for Complex128Array, Float64 index for Float64Array)
* @returns {(Complex128Array|Float64Array)} `x`
*/
function zlacgv( N, x, stride, offset ) {
	var tmp;
	var xv;
	var sx;
	var ix;
	var i;

	if ( N <= 0 ) {
		return x;
	}

	tmp = float64view( x, offset );
	xv = tmp[ 0 ];
	ix = tmp[ 1 ];

	sx = stride * 2;
	for ( i = 0; i < N; i++ ) {
		xv[ ix + 1 ] = -xv[ ix + 1 ];
		ix += sx;
	}
	return x;
}


// EXPORTS //

module.exports = zlacgv;
