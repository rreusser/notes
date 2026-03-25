

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* LAPACK dlamrg routine
*
* @param {integer} n1 - n1
* @param {integer} n2 - n2
* @param {Float64Array} a - input array
* @param {integer} strideA - stride length for `a`
* @param {NonNegativeInteger} offsetA - starting index for `a`
* @param {integer} dtrd1 - dtrd1
* @param {integer} dtrd2 - dtrd2
* @param {Int32Array} INDEX - output array
* @param {integer} strideINDEX - stride length for `INDEX`
* @param {NonNegativeInteger} offsetINDEX - starting index for `INDEX`
*/
function dlamrg( n1, n2, a, strideA, offsetA, dtrd1, dtrd2, INDEX, strideINDEX, offsetINDEX ) { // eslint-disable-line max-len, max-params
	return base( n1, n2, a, strideA, offsetA, dtrd1, dtrd2, INDEX, strideINDEX, offsetINDEX ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dlamrg;
