'use strict';

var Float64Array = require( '@stdlib/array/float64' );
var dzasum = require( './../lib/base.js' );

// Sum of |Re| + |Im| for each complex element:
var zx = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );

var result = dzasum( 2, zx, 1, 0 );
console.log( result ); // eslint-disable-line no-console
// => 10.0
