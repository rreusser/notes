/* eslint-disable camelcase */

'use strict';

var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dsyconvf_rook = require( './../lib' );

// 2x2 symmetric factor (column-major) with a 2x2 rook pivot:
var A = new Float64Array( [ 1.0, 0.0, 3.0, 2.0 ] );
var E = new Float64Array( 2 );
var IPIV = new Int32Array( [ -1, -2 ] );

var info = dsyconvf_rook.ndarray( 'upper', 'convert', 2, A, 1, 2, 0, E, 1, 0, IPIV, 1, 0 );
console.log( info ); // eslint-disable-line no-console
