
'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Provides error bounds for the solution to a system with a complex triangular band matrix (BLAS/LAPACK-style API placeholder).
*
* @param {*} args - arguments
* @returns {integer} info status code
*/
function ztbrfs( args ) {
	return base( args );
}


// EXPORTS //

module.exports = ztbrfs;
