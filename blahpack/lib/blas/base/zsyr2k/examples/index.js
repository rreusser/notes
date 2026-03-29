'use strict';

var Float64Array = require( '@stdlib/array/float64' );
var zsyr2k = require( './../lib/base.js' );

// Perform C = alpha*A*B' + alpha*B*A' + beta*C:
var A = new Float64Array( [ 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0 ] );
var B = new Float64Array( [ 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0 ] );
var C = new Float64Array( 8 );
var alpha = new Float64Array( [ 1.0, 0.0 ] );
var beta = new Float64Array( [ 0.0, 0.0 ] );

zsyr2k( 'upper', 'no-transpose', 2, 2, alpha, A, 2, 1, 0, B, 2, 1, 0, beta, C, 2, 1, 0 );
console.log( C ); // eslint-disable-line no-console
