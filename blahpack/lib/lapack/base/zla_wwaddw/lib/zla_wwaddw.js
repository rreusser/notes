

'use strict';

// MODULES //

var format = require( '@stdlib/string/format' );
var base = require( './base.js' );


// MAIN //

/**
* Adds a complex vector W into a doubled-single accumulation vector (X, Y).
*
* @param {NonNegativeInteger} N - length of vectors X, Y, and W
* @param {Complex128Array} x - first part of the doubled-single accumulation vector
* @param {Complex128Array} y - second part of the doubled-single accumulation vector
* @param {Complex128Array} w - vector to be added
* @returns {void}
*/
function zla_wwaddw( N, x, y, w ) { // eslint-disable-line camelcase
	if ( N < 0 ) {
		throw new RangeError( format( 'invalid argument. First argument must be a nonnegative integer. Value: `%d`.', N ) );
	}
	base( N, x, 1, 0, y, 1, 0, w, 1, 0 );
}


// EXPORTS //

module.exports = zla_wwaddw;
