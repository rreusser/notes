

'use strict';

// MODULES //

var format = require( '@stdlib/string/format' );
var base = require( './base.js' );


// MAIN //

/**
* Compute a safe BLAS-style constant for scaling matrix norms
*
* @param {number} anorm - anorm
* @param {number} bnorm - bnorm
* @param {number} cnorm - cnorm
* @returns {number} result
*/
function dlarmm( anorm, bnorm, cnorm ) {

	return base( anorm, bnorm, cnorm );
}


// EXPORTS //

module.exports = dlarmm;
