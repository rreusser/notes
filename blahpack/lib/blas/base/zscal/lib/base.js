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
* Scale a complex double-precision vector by a complex constant.
*
* @private
* @param {PositiveInteger} N - number of complex elements
* @param {(Complex128|Float64Array)} za - complex scalar
* @param {(Complex128Array|Float64Array)} zx - complex input vector
* @param {integer} strideX - stride for `zx` (in complex elements)
* @param {NonNegativeInteger} offsetX - starting index for `zx` (in complex elements for Complex128Array, Float64 index for Float64Array)
* @returns {(Complex128Array|Float64Array)} `zx`
*/
function zscal( N, za, zx, strideX, offsetX ) {
	var zaR;
	var zaI;
	var tmp;
	var xv;
	var sx;
	var ix;
	var tr;
	var i;

	if ( N <= 0 ) {
		return zx;
	}

	// Handle both Complex128 (re/im properties) and Float64Array [re, im]
	if ( typeof za === 'object' && typeof za.re === 'number' ) {
		zaR = za.re;
		zaI = za.im;
	} else {
		zaR = za[ 0 ];
		zaI = za[ 1 ];
	}

	// Early return if za === (1, 0)
	if ( zaR === 1.0 && zaI === 0.0 ) {
		return zx;
	}

	tmp = float64view( zx, offsetX );
	xv = tmp[ 0 ];
	ix = tmp[ 1 ];

	// Each complex element spans 2 doubles, so multiply stride by 2
	sx = strideX * 2;

	for ( i = 0; i < N; i++ ) {
		tr = zaR * xv[ ix ] - zaI * xv[ ix + 1 ];
		xv[ ix + 1 ] = zaR * xv[ ix + 1 ] + zaI * xv[ ix ];
		xv[ ix ] = tr;
		ix += sx;
	}
	return zx;
}


// EXPORTS //

module.exports = zscal;
