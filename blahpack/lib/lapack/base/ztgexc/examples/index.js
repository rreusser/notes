/* eslint-disable no-restricted-syntax, max-len */

'use strict';

var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var ztgexc = require( './../lib' );

// 3x3 upper triangular matrix pair (column-major, interleaved re/im):
var A = new Complex128Array( [ 2.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.5, -0.2, 4.0, 0.0, 0.0, 0.0, 0.3, 0.1, 0.7, -0.3, 6.0, -1.0 ] );
var B = new Complex128Array( [ 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.2, 0.1, 1.0, 0.0, 0.0, 0.0, 0.1, -0.05, 0.3, 0.2, 1.0, 0.0 ] );
var Q = new Complex128Array( 9 );
var Z = new Complex128Array( 9 );

// Move diagonal element from position 0 to position 2 (0-based):
var result = ztgexc.ndarray( true, true, 3, A, 1, 3, 0, B, 1, 3, 0, Q, 1, 3, 0, Z, 1, 3, 0, 0, 2 );
var Av = reinterpret( A, 0 );

console.log( 'info:', result.info ); // eslint-disable-line no-console
console.log( 'ilst:', result.ilst ); // eslint-disable-line no-console
console.log( 'A(1,1):', Av[ 0 ], '+', Av[ 1 ], 'i' ); // eslint-disable-line no-console
console.log( 'A(2,2):', Av[ 8 ], '+', Av[ 9 ], 'i' ); // eslint-disable-line no-console
