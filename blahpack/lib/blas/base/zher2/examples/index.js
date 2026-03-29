'use strict';

var Float64Array = require( '@stdlib/array/float64' );
var zher2 = require( './../lib/base.js' );

// Perform Hermitian rank-2 update of A:
var x = new Float64Array( [ 1.0, 0.0, 0.0, 1.0 ] );
var y = new Float64Array( [ 1.0, 0.0, 0.0, 1.0 ] );
var A = new Float64Array( [ 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0 ] );
var alpha = new Float64Array( [ 1.0, 0.0 ] );

zher2( 'upper', 2, alpha, x, 1, 0, y, 1, 0, A, 2, 1, 0 );
console.log( A ); // eslint-disable-line no-console
