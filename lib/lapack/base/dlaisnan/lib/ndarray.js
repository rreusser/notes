/**
 * Tests for NaN by comparing two arguments for inequality.
 *
 *
 * @param {number} din1 - first value
 * @param {number} din2 - second value
 * @returns {boolean} true if din1 !== din2
 */

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Tests for NaN by comparing two arguments for inequality.
*
* @param {number} din1 - first value
* @param {number} din2 - second value
* @returns {boolean} true if din1 !== din2
*/
function dlaisnan( din1, din2 ) {
	return base( din1, din2 );
}


// EXPORTS //

module.exports = dlaisnan;
