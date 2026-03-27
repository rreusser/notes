

'use strict';

// MODULES //

var stride2offset = require( '@stdlib/strided/base/stride2offset' );
var base = require( './base.js' );


// MAIN //

/**
* @license Apache-2.0
*
* @param {NonNegativeInteger} N - N
* @param {Complex128Array} CX - CX
* @param {integer} strideCX - strideCX
* @returns {*} result
*/
function dzsum1( N, CX, strideCX ) {
	var ocx;

	ocx = stride2offset( N, strideCX );
	return base( N, CX, strideCX, ocx );
}


// EXPORTS //

module.exports = dzsum1;
