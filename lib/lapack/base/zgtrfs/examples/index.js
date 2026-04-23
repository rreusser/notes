
'use strict';

var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zgttrf = require( './../../zgttrf/lib' );
var zgttrs = require( './../../zgttrs/lib' );
var zgtrfs = require( './../lib' );

var n = 4;
var DL = new Complex128Array( [ 2, 1, 1, -1, 3, 0.5 ] );
var d = new Complex128Array( [ 4, 1, 5, 2, 3, 1, 6, -1 ] );
var DU = new Complex128Array( [ 1, 0.5, -1, 1, 2, 1 ] );
var DLF = new Complex128Array( [ 2, 1, 1, -1, 3, 0.5 ] );
var DF = new Complex128Array( [ 4, 1, 5, 2, 3, 1, 6, -1 ] );
var DUF = new Complex128Array( [ 1, 0.5, -1, 1, 2, 1 ] );
var DU2 = new Complex128Array( n );
var IPIV = new Int32Array( n );
var B = new Complex128Array( [ 5, 1.5, 6, 4, 6, 1, 9, -0.5 ] );
var X = new Complex128Array( [ 5, 1.5, 6, 4, 6, 1, 9, -0.5 ] );
var WORK = new Complex128Array( 2 * n );
var RWORK = new Float64Array( n );
var FERR = new Float64Array( 1 );
var BERR = new Float64Array( 1 );
var info;

zgttrf.ndarray( n, DLF, 1, 0, DF, 1, 0, DUF, 1, 0, DU2, 1, 0, IPIV, 1, 0 );
zgttrs.ndarray( 'no-transpose', n, 1, DLF, 1, 0, DF, 1, 0, DUF, 1, 0, DU2, 1, 0, IPIV, 1, 0, X, 2, n * 2, 0 ); // eslint-disable-line max-len
info = zgtrfs.ndarray( 'no-transpose', n, 1, DL, 1, 0, d, 1, 0, DU, 1, 0, DLF, 1, 0, DF, 1, 0, DUF, 1, 0, DU2, 1, 0, IPIV, 1, 0, B, 1, n, 0, X, 1, n, 0, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, RWORK, 1, 0 ); // eslint-disable-line max-len

console.log( 'info:', info ); // eslint-disable-line no-console
console.log( 'X (Float64 view):', reinterpret( X, 0 ) ); // eslint-disable-line no-console
console.log( 'FERR:', FERR ); // eslint-disable-line no-console
console.log( 'BERR:', BERR ); // eslint-disable-line no-console
