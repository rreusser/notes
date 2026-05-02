

'use strict';

/**
* Compute a safe BLAS-style constant for scaling matrix norms
*
* @module @stdlib/lapack/base/dlarmm
*
*
* @example
* var dlarmm = require( '@stdlib/lapack/base/dlarmm' );
*
* dlarmm( 1.0, 1.0, 1.0 );
* dlarmm( 1e308, 1.0, 1e307 );
*/

// MODULES //

var main = require( './main.js' );


// EXPORTS //

module.exports = main;

// exports: { "ndarray": "dlarmm.ndarray" }
