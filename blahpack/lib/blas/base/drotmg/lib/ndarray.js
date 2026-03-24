/* eslint-disable max-len, max-params */

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Constructs a modified Givens plane rotation.
*
* @param {Float64Array} D - two-element array containing `[dd1, dd2]`
* @param {integer} strideD - stride length for `D`
* @param {NonNegativeInteger} offsetD - starting index for `D`
* @param {Float64Array} x1 - one-element array containing `dx1`
* @param {integer} strideX1 - stride length for `x1`
* @param {NonNegativeInteger} offsetX1 - starting index for `x1`
* @param {number} dy1 - scalar value `dy1`
* @param {Float64Array} param - five-element output array for the rotation parameters
* @param {integer} strideParam - stride length for `param`
* @param {NonNegativeInteger} offsetParam - starting index for `param`
* @returns {void}
*/
function drotmg( D, strideD, offsetD, x1, strideX1, offsetX1, dy1, param, strideParam, offsetParam ) {
	return base( D, strideD, offsetD, x1, strideX1, offsetX1, dy1, param, strideParam, offsetParam );
}


// EXPORTS //

module.exports = drotmg;
