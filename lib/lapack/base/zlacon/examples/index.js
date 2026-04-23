
'use strict';

var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var zlacon = require( './../lib' );

var N = 3;
var V = new Complex128Array( N );
var X = new Complex128Array( N );
var EST = new Float64Array( 1 );
var KASE = new Int32Array( 1 );

KASE[ 0 ] = 0;

// First call initializes X and requests A*X (KASE=1):
zlacon.ndarray( N, V, 1, 0, X, 1, 0, EST, KASE );
console.log( 'After init: KASE =', KASE[ 0 ] ); // 1
