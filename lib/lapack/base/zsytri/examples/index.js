'use strict';

var Complex128Array = require( '@stdlib/array/complex128' );
var Int32Array = require( '@stdlib/array/int32' ); // eslint-disable-line stdlib/require-globals
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zsytri = require( './../lib' );

// 1x1 complex symmetric matrix A = [4+1i], already factored (trivial 1x1 pivot):
var A = new Complex128Array( [ 4.0, 1.0 ] );
var IPIV = new Int32Array( [ 0 ] );
var WORK = new Complex128Array( 1 );
var Av = reinterpret( A, 0 );

var info = zsytri.ndarray( 'upper', 1, A, 1, 1, 0, IPIV, 1, 0, WORK, 1, 0 );

console.log( 'info:', info ); // eslint-disable-line no-console
// => 0

console.log( 'A inverse:', Av[ 0 ], '+', Av[ 1 ], 'i' ); // eslint-disable-line no-console
// => 0.23529... + -0.05882... i
