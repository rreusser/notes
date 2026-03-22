/**
* @license Apache-2.0
*
* Returns a Float64Array view of a complex array and converts an offset
* from complex element units to Float64 units when needed.
*
* If the input is already a Float64Array, returns it directly and the
* offset is assumed to already be in Float64 units (unchanged).
* If the input is a Complex128Array, reinterprets it as a Float64Array
* and converts the offset from complex elements to Float64 index (* 2).
*
* This allows BLAS/LAPACK routines to accept both Complex128Array
* (public API) and Float64Array (internal interleaved storage) inputs.
*/

'use strict';

// MODULES //

var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );

// MAIN //

/**
* Return a Float64Array view and converted offset.
*
* @param {(Complex128Array|Float64Array)} x - input array
* @param {NonNegativeInteger} offset - starting index
* @returns {Array} two-element array: [ Float64Array view, Float64 offset ]
*/
function float64view( x, offset ) {
	if ( x instanceof Float64Array ) {
		return [ x, offset ];
	}
	return [ reinterpret( x, 0 ), offset * 2 ];
}

/**
* Extract real and imaginary parts from a complex scalar.
* Handles both Complex128 objects (with .re/.im) and Float64Array [re, im].
*
* @param {(Complex128|Float64Array)} z - complex scalar
* @returns {Array} two-element array: [ real, imag ]
*/
function complexParts( z ) {
	if ( typeof z === 'object' && typeof z.re === 'number' ) {
		return [ z.re, z.im ];
	}
	return [ z[ 0 ], z[ 1 ] ];
}


// EXPORTS //

module.exports = float64view;
module.exports.complexParts = complexParts;
