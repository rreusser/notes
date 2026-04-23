
'use strict';

var Float64Array = require( '@stdlib/array/float64' );
var Uint8Array = require( '@stdlib/array/uint8' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var ztrevc = require( './../lib' );

// 3x3 upper triangular matrix T (column-major):
var T = new Complex128Array([
	2,
	1,
	0,
	0,
	0,
	0,
	1,
	0.5,
	3,
	-1,
	0,
	0,
	0.5,
	-0.5,
	1,
	1,
	4,
	0.5
]);

var VR = new Complex128Array( 9 );
var VL = new Complex128Array( 9 );
var WORK = new Complex128Array( 6 );
var RWORK = new Float64Array( 3 );

// Compute both right and left eigenvectors:
var info = ztrevc( 'column-major', 'both', 'all', new Uint8Array( 3 ), 1, 3, T, 3, VL, 3, VR, 3, 3, 0, WORK, 1, RWORK, 1 );

var vrv = reinterpret( VR, 0 );
var vlv = reinterpret( VL, 0 );
console.log( 'info:', info ); // eslint-disable-line no-console
console.log( 'VR[0]:', vrv[ 0 ], vrv[ 1 ] ); // eslint-disable-line no-console
console.log( 'VL[0]:', vlv[ 0 ], vlv[ 1 ] ); // eslint-disable-line no-console
