
'use strict';

var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var zlansp = require( './../lib' );

/*
* 3x3 complex symmetric matrix in upper packed storage:
*   A = [ (2,1)   (1,2)   (3,-1) ]
*       [ (1,2)   (5,-1)  (2,1)  ]
*       [ (3,-1)  (2,1)   (4,2)  ]
*/
var AP = new Complex128Array( [ 2.0, 1.0, 1.0, 2.0, 5.0, -1.0, 3.0, -1.0, 2.0, 1.0, 4.0, 2.0 ] ); // eslint-disable-line max-len
var WORK = new Float64Array( 3 );

var result = zlansp( 'max', 'upper', 3, AP, WORK );
console.log( 'max norm: %d', result );

WORK = new Float64Array( 3 );
result = zlansp( 'one-norm', 'upper', 3, AP, WORK );
console.log( 'one-norm: %d', result );

result = zlansp( 'frobenius', 'upper', 3, AP, WORK );
console.log( 'frobenius norm: %d', result );
