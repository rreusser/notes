'use strict';

var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var zhpcon = require( './../lib' );

// 3x3 identity in upper packed format (already factored, trivial pivots)
var AP = new Complex128Array( [ 1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0 ] ); // eslint-disable-line max-len
var IPIV = new Int32Array( [ 0, 1, 2 ] );
var rcond = new Float64Array( 1 );
var WORK = new Complex128Array( 6 );

var info = zhpcon.ndarray( 'upper', 3, AP, 1, 0, IPIV, 1, 0, 1.0, rcond, WORK, 1, 0 ); // eslint-disable-line max-len
console.log( 'info:', info );
// => info: 0

console.log( 'rcond:', rcond[ 0 ] );
// => rcond: 1
