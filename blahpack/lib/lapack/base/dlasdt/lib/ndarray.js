
'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Create a tree of subproblems for bidiagonal divide and conquer.
*
* @param {NonNegativeInteger} N - number of columns
* @param {integer} lvl - lvl
* @param {integer} nd - nd
* @param {Int32Array} INODE - input array
* @param {integer} strideINODE - stride length for `INODE`
* @param {NonNegativeInteger} offsetINODE - starting index for `INODE`
* @param {Int32Array} NDIML - input array
* @param {integer} strideNDIML - stride length for `NDIML`
* @param {NonNegativeInteger} offsetNDIML - starting index for `NDIML`
* @param {Int32Array} NDIMR - output array
* @param {integer} strideNDIMR - stride length for `NDIMR`
* @param {NonNegativeInteger} offsetNDIMR - starting index for `NDIMR`
* @param {integer} msub - msub
*/
function dlasdt( N, lvl, nd, INODE, strideINODE, offsetINODE, NDIML, strideNDIML, offsetNDIML, NDIMR, strideNDIMR, offsetNDIMR, msub ) { // eslint-disable-line max-len, max-params
	return base( N, lvl, nd, INODE, strideINODE, offsetINODE, NDIML, strideNDIML, offsetNDIML, NDIMR, strideNDIMR, offsetNDIMR, msub ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dlasdt;
