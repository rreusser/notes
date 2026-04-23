
'use strict';

var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zgesc2 = require( './../lib' );

// A 2x2 LU-factored complex matrix (column-major, from zgetc2):
var A = new Complex128Array( [ 4.0, 1.0, 0.353, 0.412, 2.0, -1.0, 1.882, 0.029 ] ); // eslint-disable-line max-len
var RHS = new Complex128Array( [ 10.0, 3.0, 7.0, 4.0 ] );
var IPIV = new Int32Array( [ 0, 1 ] );
var JPIV = new Int32Array( [ 0, 1 ] );
var scale = new Float64Array( 1 );
var view;

zgesc2.ndarray( 2, A, 1, 2, 0, RHS, 1, 0, IPIV, 1, 0, JPIV, 1, 0, scale ); // eslint-disable-line max-len

view = reinterpret( RHS, 0 );
console.log( 'Solution (re/im interleaved):', Array.prototype.slice.call( view ) ); // eslint-disable-line no-console, max-len
console.log( 'Scale:', scale[ 0 ] ); // eslint-disable-line no-console
