
'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Computes the parameters for a Givens rotation.
*
* @param {Float64Array} a - first complex number (real, imag)
* @param {NonNegativeInteger} offsetA - starting index for `a`
* @param {Float64Array} b - second complex number (real, imag)
* @param {NonNegativeInteger} offsetB - starting index for `b`
* @param {Float64Array} c - output cosine
* @param {NonNegativeInteger} offsetC - starting index for `c`
* @param {Float64Array} s - output sine (real, imag)
* @param {NonNegativeInteger} offsetS - starting index for `s`
* @returns {void}
*/
function zrotg( a, offsetA, b, offsetB, c, offsetC, s, offsetS ) {
	return base( a, offsetA, b, offsetB, c, offsetC, s, offsetS );
}


// EXPORTS //

module.exports = zrotg;
