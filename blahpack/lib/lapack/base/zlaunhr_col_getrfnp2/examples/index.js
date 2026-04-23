/* eslint-disable camelcase, stdlib/require-file-extensions, max-len */

'use strict';

var Complex128Array = require( '@stdlib/array/complex128' );
var zlaunhr_col_getrfnp2 = require( '@stdlib/lapack/base/zlaunhr_col_getrfnp2' );

var M = 3;
var N = 3;

var A = new Complex128Array( [ 0.5, 0.3, 0.2, -0.1, -0.3, 0.2, -0.1, 0.4, 0.6, -0.2, 0.1, 0.3, 0.2, -0.3, -0.4, 0.1, 0.5, 0.2 ] );
var D = new Complex128Array( 3 );

zlaunhr_col_getrfnp2( 'column-major', M, N, A, M, D, 1 );
console.log( A ); // eslint-disable-line no-console
console.log( D ); // eslint-disable-line no-console
