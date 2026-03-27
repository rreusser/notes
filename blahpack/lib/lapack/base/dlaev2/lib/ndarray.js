/**
 * Computes the eigendecomposition of a 2-by-2 symmetric matrix:.
 *
 * \[ a  b \]
 * \[ b  c \]
 *
 * On return:
 *
 * -   rt1: eigenvalue of larger absolute value
 * -   rt2: eigenvalue of smaller absolute value
 * -   cs1: cosine of the rotation
 * -   sn1: sine of the rotation
 *
 * The rotation matrix \[cs1, sn1; -sn1, cs1\] diagonalizes the input:
 * \[ cs1  sn1 \] \[ a  b \] \[ cs1 -sn1 \] = \[ rt1  0  \]
 * \[-sn1  cs1 \] \[ b  c \] \[ sn1  cs1 \]   \[  0  rt2 \]
 *
 *
 * @param {number} a - (1,1) element of the 2-by-2 matrix
 * @param {number} b - (1,2) element (also (2,1)) of the 2-by-2 matrix
 * @param {number} c - (2,2) element of the 2-by-2 matrix
 * @returns {Object} object with `rt1`, `rt2`, `cs1`, and `sn1` properties
 */

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Computes the eigendecomposition of a 2-by-2 symmetric matrix:.
*
* @param {number} a - (1,1) element of the 2-by-2 matrix
* @param {number} b - (1,2) element (also (2,1)) of the 2-by-2 matrix
* @param {number} c - (2,2) element of the 2-by-2 matrix
* @returns {Object} object with `rt1`, `rt2`, `cs1`, and `sn1` properties
*/
function dlaev2( a, b, c ) {
	return base( a, b, c );
}


// EXPORTS //

module.exports = dlaev2;
