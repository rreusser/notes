
'use strict';

var Complex128Array = require( '@stdlib/array/complex128' );
var Int32Array = require( '@stdlib/array/int32' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zspsv = require( './../lib' );

// 3x3 complex symmetric matrix (upper packed):
// A = [ (4,1)   (2,-1)   (1,2);
//       (2,-1)  (5,0.5)  (3,-1);
//       (1,2)   (3,-1)   (6,1) ]
var AP = new Complex128Array([
	4.0, 1.0, 2.0, -1.0, 5.0, 0.5, 1.0, 2.0, 3.0, -1.0, 6.0, 1.0
]);
var IPIV = new Int32Array( 3 );

// Right-hand side: b = A * [1; 1; 1]
var B = new Complex128Array([ 7.0, 2.0, 10.0, -1.5, 10.0, 2.0 ]);

var info = zspsv.ndarray( 'upper', 3, 1, AP, 1, 0, IPIV, 1, 0, B, 1, 3, 0 );
console.log( 'info:', info );

var view = reinterpret( B, 0 );
console.log( 'Solution x:' );
console.log( '  x[0] =', view[ 0 ], '+', view[ 1 ], 'i' );
console.log( '  x[1] =', view[ 2 ], '+', view[ 3 ], 'i' );
console.log( '  x[2] =', view[ 4 ], '+', view[ 5 ], 'i' );
