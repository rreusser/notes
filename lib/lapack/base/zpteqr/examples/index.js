'use strict';

var Float64Array = require( '@stdlib/array/float64' );
var Complex128Array = require( '@stdlib/array/complex128' );
var zpteqr = require( './../lib' );

var d = new Float64Array( [ 4.0, 4.0, 4.0 ] );
var e = new Float64Array( [ 1.0, 1.0 ] );
var Z = new Complex128Array( 9 );
var WORK = new Float64Array( 12 );

var info = zpteqr( 'column-major', 'initialize', 3, d, 1, e, 1, Z, 3, WORK, 1 );

console.log( 'info:', info );
console.log( 'eigenvalues:', d );
