'use strict';

var Float64Array = require( '@stdlib/array/float64' );
var dlaneg = require( './../lib' );

// Build a small symmetric tridiagonal LDL^T representation:
var d = new Float64Array( [ 4.0, 3.0, 2.0, 1.0, 5.0 ] );
var lld = new Float64Array( [ 0.5, 0.5, 0.5, 0.5 ] );

// Compute the Sturm count (number of negative pivots of T - sigma*I):
var negcnt = dlaneg( 5, d, 1, lld, 1, 10.0, 1e-30, 3 );
console.log( negcnt ); // eslint-disable-line no-console
