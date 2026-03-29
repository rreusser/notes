'use strict';

var Float64Array = require( '@stdlib/array/float64' );
var zhpr = require( './../lib/base.js' );

// Perform A = alpha*x*conjg(x') + A for Hermitian packed A:
var x = new Float64Array( [ 1.0, 0.0, 0.0, 1.0 ] );
var AP = new Float64Array( [ 1.0, 0.0, 0.0, 0.0, 1.0, 0.0 ] );

zhpr( 'upper', 2, 1.0, x, 1, 0, AP, 1, 0 );
console.log( AP ); // eslint-disable-line no-console
