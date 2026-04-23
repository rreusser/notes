'use strict';

var Float64Array = require( '@stdlib/array/float64' );
var ztrmv = require( './../lib/base.js' );

// Perform x = A*x for complex triangular A:
var A = new Float64Array( [ 2.0, 0.0, 3.0, 0.0, 0.0, 0.0, 4.0, 0.0 ] );
var x = new Float64Array( [ 1.0, 0.0, 1.0, 0.0 ] );

ztrmv( 'upper', 'no-transpose', 'non-unit', 2, A, 2, 1, 0, x, 1, 0 );
console.log( x ); // eslint-disable-line no-console
