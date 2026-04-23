'use strict';

var Float64Array = require( '@stdlib/array/float64' );
var zher = require( './../lib/base.js' );

// Perform A = alpha*x*conjg(x') + A for Hermitian A:
var x = new Float64Array( [ 1.0, 0.0, 0.0, 1.0 ] );
var A = new Float64Array( [ 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0 ] );

zher( 'upper', 2, 1.0, x, 1, 0, A, 2, 1, 0 );
console.log( A ); // eslint-disable-line no-console
