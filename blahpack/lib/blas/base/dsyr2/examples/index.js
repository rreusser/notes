'use strict';

var Float64Array = require( '@stdlib/array/float64' );
var dsyr2 = require( './../lib/base.js' );

// Perform A = alpha*x*y' + alpha*y*x' + A:
var x = new Float64Array( [ 1.0, 2.0 ] );
var y = new Float64Array( [ 3.0, 4.0 ] );
var A = new Float64Array( [ 1.0, 0.0, 0.0, 1.0 ] );

dsyr2( 'upper', 2, 1.0, x, 1, 0, y, 1, 0, A, 2, 1, 0 );
console.log( A ); // eslint-disable-line no-console
