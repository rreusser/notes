/**
* @license Apache-2.0
*
* Copyright (c) 2025 Ricky Reusser.
*/

'use strict';

var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zlag2c = require( './../lib' );

var M = 3;
var N = 3;
var A = new Complex128Array( M * N );
var SA = new Complex128Array( M * N );
var v = reinterpret( A, 0 );
var i;
var info;

for ( i = 0; i < 2 * M * N; i++ ) {
	v[ i ] = ( i + 1 ) * 0.5;
}
info = zlag2c( 'column-major', M, N, A, M, SA, M );
console.log( 'info = %d', info ); // eslint-disable-line no-console
console.log( reinterpret( SA, 0 ) ); // eslint-disable-line no-console
