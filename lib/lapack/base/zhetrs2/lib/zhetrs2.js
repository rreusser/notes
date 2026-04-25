

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* @license Apache-2.0
*
* @param {number} ar - ar
* @param {number} ai - ai
* @param {number} br - br
* @param {number} bi - bi
* @returns {integer} info status code
*/
function zhetrs2( ar, ai, br, bi ) {
	return base( ar, ai, br, bi );
}


// EXPORTS //

module.exports = zhetrs2;
