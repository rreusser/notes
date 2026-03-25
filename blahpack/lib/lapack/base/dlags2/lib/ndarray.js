

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Computes 2-by-2 orthogonal matrices U, V, Q for simultaneous upper/lower triangularization
*
* @param {boolean} upper - upper
* @param {number} a1 - a1
* @param {number} a2 - a2
* @param {number} a3 - a3
* @param {number} b1 - b1
* @param {number} b2 - b2
* @param {number} b3 - b3
* @param {number} csu - csu
* @param {number} snu - snu
* @param {number} csv - csv
* @param {number} snv - snv
* @param {number} csq - csq
* @param {number} snq - snq
*/
function dlags2( upper, a1, a2, a3, b1, b2, b3, csu, snu, csv, snv, csq, snq ) { // eslint-disable-line max-len, max-params
	return base( upper, a1, a2, a3, b1, b2, b3, csu, snu, csv, snv, csq, snq ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dlags2;
