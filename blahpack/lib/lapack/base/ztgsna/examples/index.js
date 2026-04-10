
'use strict';

var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var Uint8Array = require( '@stdlib/array/uint8' );
var Int32Array = require( '@stdlib/array/int32' );
var ztgsna = require( './../lib/base.js' );

var N = 1;
var A = new Complex128Array( [ 3.0, 2.0 ] );
var B = new Complex128Array( [ 1.0, 0.5 ] );
var VL = new Complex128Array( [ 1.0, 0.0 ] );
var VR = new Complex128Array( [ 1.0, 0.0 ] );
var SELECT = new Uint8Array( N );
var s = new Float64Array( N );
var DIF = new Float64Array( N );
var WORK = new Complex128Array( 1 );
var IWORK = new Int32Array( 1 );

var res = ztgsna( 'both', 'all', SELECT, 1, 0, N, A, 1, N, 0, B, 1, N, 0, VL, 1, N, 0, VR, 1, N, 0, s, 1, 0, DIF, 1, 0, N, 0, WORK, 1, 0, 0, IWORK, 1, 0 );
console.log( 's[0]   =', s[ 0 ] ); // eslint-disable-line no-console
console.log( 'DIF[0] =', DIF[ 0 ] ); // eslint-disable-line no-console
console.log( 'm      =', res.m ); // eslint-disable-line no-console
