
'use strict';

var Complex128Array = require( '@stdlib/array/complex128' ); // eslint-disable-line stdlib/require-file-extensions
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' ); // eslint-disable-line stdlib/require-file-extensions
var zpptri = require( './../lib' ); // eslint-disable-line stdlib/require-file-extensions

// 2x2 upper Cholesky factor, packed column-major: [ (3,0), (1,0), (2,0) ]
var AP = new Complex128Array( [ 3.0, 0.0, 1.0, 0.0, 2.0, 0.0 ] );

var info = zpptri( 'upper', 2, AP );
console.log( 'info:', info ); // eslint-disable-line no-console
console.log( 'inverse (packed, interleaved re/im):', reinterpret( AP, 0 ) ); // eslint-disable-line no-console
