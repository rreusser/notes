/**
 * Compute |re|^2 + |im|^2.
 *
 *
 * @param {number} re - real part
 * @param {number} im - imaginary part
 * @returns {number} sum of squares
 */

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Generates a plane rotation so that:.
*
* @param {Float64Array} f - first component [re, im]
* @param {Float64Array} g - second component [re, im]
* @param {Float64Array} out - output: out[0]=c, out[1..2]=s (re,im), out[3..4]=r (re,im)
* @returns {Float64Array} out
*/
function zlartg( f, g, out ) {
	return base( f, g, out );
}


// EXPORTS //

module.exports = zlartg;
