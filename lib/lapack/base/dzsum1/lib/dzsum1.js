
'use strict';

// MODULES //

var stride2offset = require( '@stdlib/strided/base/stride2offset' );
var format = require( '@stdlib/string/format' );
var base = require( './base.js' );


// MAIN //

/**
* @license Apache-2.0
*
* @param {NonNegativeInteger} N - N
* @param {Complex128Array} CX - CX
* @param {integer} strideCX - strideCX
* @returns {Float64Array} output array
*/
function dzsum1( N, CX, strideCX ) {
	var ocx;

	ocx = stride2offset( N, strideCX );
	if ( N < 0 ) {
		throw new RangeError( format( 'invalid argument. First argument must be a nonnegative integer. Value: `%d`.', N ) );
	}
	return base( N, CX, strideCX, ocx );
}


// EXPORTS //

module.exports = dzsum1;
