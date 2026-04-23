'use strict';

var Float64Array = require( '@stdlib/array/float64' );
var dtbmv = require( './../lib/base.js' );

// Perform x = A*x for triangular banded A:
var A = new Float64Array( [ 0.0, 1.0, 2.0, 3.0 ] );
var x = new Float64Array( [ 1.0, 1.0 ] );

dtbmv( 'upper', 'no-transpose', 'non-unit', 2, 1, A, 2, 1, 0, x, 1, 0 );
console.log( x ); // eslint-disable-line no-console
