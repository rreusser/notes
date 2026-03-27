/**
 * Computes the eigenvalues of a 2-by-2 symmetric matrix:.
 *
 * \[ a  b \]
 * \[ b  c \]
 *
 * On return, rt1 is the eigenvalue of larger absolute value, and rt2 is the
 * eigenvalue of smaller absolute value.
 *
 * Uses the formula:
 * rt1 = (sm + rt) / 2   or  (sm - rt) / 2  depending on sign of sm
 * `rt2 = (acmx / rt1)*acmn - (b / rt1)*b`
 * where sm = a + c, df = a - c, and rt = sqrt(df^2 + 4*b^2).
 *
 *
 * @param {number} a - (1,1) element of the 2-by-2 matrix
 * @param {number} b - (1,2) element (also (2,1)) of the 2-by-2 matrix
 * @param {number} c - (2,2) element of the 2-by-2 matrix
 * @returns {Object} object with `rt1` and `rt2` eigenvalues
 */

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Computes the eigenvalues of a 2-by-2 symmetric matrix:.
*
* @param {number} a - (1,1) element of the 2-by-2 matrix
* @param {number} b - (1,2) element (also (2,1)) of the 2-by-2 matrix
* @param {number} c - (2,2) element of the 2-by-2 matrix
* @returns {Object} object with `rt1` and `rt2` eigenvalues
*/
function dlae2( a, b, c ) {
	return base( a, b, c );
}


// EXPORTS //

module.exports = dlae2;
