
'use strict';

var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zlargv = require( './../lib' );

var x = new Complex128Array( [ 3.0, 1.0, 1.0, 2.0 ] );
var y = new Complex128Array( [ 4.0, 0.0, 3.0, 1.0 ] );
var c = new Float64Array( 2 );

zlargv.ndarray( 2, x, 1, 0, y, 1, 0, c, 1, 0 );

console.log( 'cosines:', c );
console.log( 'x (r):', reinterpret( x, 0 ) );
console.log( 'y (sines):', reinterpret( y, 0 ) );
