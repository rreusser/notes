'use strict';

var Float64Array = require( '@stdlib/array/float64' );
var dspr = require( './../lib/base.js' );

// Perform A = alpha*x*x' + A for symmetric packed A:
var x = new Float64Array( [ 1.0, 2.0 ] );
var AP = new Float64Array( [ 1.0, 0.0, 1.0 ] );

dspr( 'upper', 2, 1.0, x, 1, 0, AP, 1, 0 );
console.log( AP ); // eslint-disable-line no-console
