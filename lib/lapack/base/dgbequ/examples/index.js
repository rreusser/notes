
'use strict';

var Float64Array = require( '@stdlib/array/float64' );
var dgbequ = require( './../lib' );

// Diagonal real band matrix (KL=0, KU=0):
var AB = new Float64Array( [ 3, 1, 2 ] );
var r = new Float64Array( 3 );
var c = new Float64Array( 3 );
var out = dgbequ.ndarray( 3, 3, 0, 0, AB, 1, 1, 0, r, 1, 0, c, 1, 0 ); // eslint-disable-line max-len

console.log( 'info:', out.info );   // eslint-disable-line no-console
console.log( 'amax:', out.amax );   // eslint-disable-line no-console
console.log( 'r:', r );             // eslint-disable-line no-console
console.log( 'c:', c );             // eslint-disable-line no-console
