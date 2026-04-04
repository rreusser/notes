
'use strict';

var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var zgbequ = require( './../lib' );

// Diagonal complex band matrix (KL=0, KU=0):
var AB = new Complex128Array( [ 3, 4, 1, 0, 0, 2 ] );
var r = new Float64Array( 3 );
var c = new Float64Array( 3 );
var out = zgbequ.ndarray( 3, 3, 0, 0, AB, 1, 1, 0, r, 1, 0, c, 1, 0 ); // eslint-disable-line max-len

console.log( 'info:', out.info );   // eslint-disable-line no-console
console.log( 'amax:', out.amax );   // eslint-disable-line no-console
console.log( 'r:', r );             // eslint-disable-line no-console
console.log( 'c:', c );             // eslint-disable-line no-console
