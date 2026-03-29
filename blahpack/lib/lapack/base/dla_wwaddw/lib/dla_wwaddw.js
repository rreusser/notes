
'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Adds a vector W into a doubled-single accumulation vector (X, Y).
*
* @param {NonNegativeInteger} N - length of vectors X, Y, and W
* @param {Float64Array} x - first part of the doubled-single accumulation vector
* @param {Float64Array} y - second part of the doubled-single accumulation vector
* @param {Float64Array} w - vector to be added
* @returns {void}
*/
function dla_wwaddw( N, x, y, w ) { // eslint-disable-line camelcase
	base( N, x, 1, 0, y, 1, 0, w, 1, 0 );
}


// EXPORTS //

module.exports = dla_wwaddw;
