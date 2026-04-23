'use strict';

var Float64Array = require( '@stdlib/array/float64' );
var dtrsm = require( './../lib/base.js' );

// Solve A*X = alpha*B for triangular A:
var A = new Float64Array( [ 2.0, 0.0, 3.0, 4.0 ] );
var B = new Float64Array( [ 4.0, 0.0, 11.0, 4.0 ] );

dtrsm( 'left', 'lower', 'no-transpose', 'non-unit', 2, 2, 1.0, A, 2, 1, 0, B, 2, 1, 0 );
console.log( B ); // eslint-disable-line no-console
