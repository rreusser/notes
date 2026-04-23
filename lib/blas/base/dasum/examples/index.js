'use strict';

var Float64Array = require( '@stdlib/array/float64' );
var dasum = require( './../lib/base.js' );

var x = new Float64Array( [ 1.0, -2.0, 3.0, -4.0, 5.0 ] );

// Compute the sum of absolute values:
var sum = dasum( 5, x, 1, 0 );
console.log( 'sum = %d', sum ); // eslint-disable-line no-console
// => sum = 15
