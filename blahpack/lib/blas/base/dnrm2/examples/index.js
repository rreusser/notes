'use strict';

var Float64Array = require( '@stdlib/array/float64' );
var dnrm2 = require( './../lib/base.js' );

// Compute the Euclidean norm of a vector:
var x = new Float64Array( [ 3.0, 4.0 ] );

var result = dnrm2( 2, x, 1, 0 );
console.log( result ); // eslint-disable-line no-console
// => 5.0
