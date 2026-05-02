
'use strict';

var Complex128Array = require( '@stdlib/array/complex128' );
var Int32Array = require( '@stdlib/array/int32' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zgetc2 = require( './../lib' );

// 3x3 complex matrix (real-only entries) in column-major order:
var N = 3;
var A = new Complex128Array( N * N );
var view = reinterpret( A, 0 );
view[ 0 ] = 4.0; view[ 2 ] = 1.0; view[ 4 ] = 2.0; // column 0
view[ 6 ] = 1.0; view[ 8 ] = 5.0; view[ 10 ] = 1.0; // column 1
view[ 12 ] = 2.0; view[ 14 ] = 1.0; view[ 16 ] = 6.0; // column 2

var IPIV = new Int32Array( N );
var JPIV = new Int32Array( N );

var info = zgetc2( N, A, N, IPIV, 1, JPIV, 1 );
console.log( 'info =', info ); // eslint-disable-line no-console
console.log( 'IPIV =', IPIV ); // eslint-disable-line no-console
console.log( 'JPIV =', JPIV ); // eslint-disable-line no-console
