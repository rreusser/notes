
'use strict';

var uniform = require( '@stdlib/random/array/uniform' );
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var zlaHeamv = require( './../lib' ); // eslint-disable-line stdlib/require-file-extensions

var opts = {
	'dtype': 'float64'
};

var N = 3;
var A = new Complex128Array( uniform( 2 * N * N, -10, 10, opts ) );
var x = new Complex128Array( uniform( 2 * N, -10, 10, opts ) );
var y = new Float64Array( N );

zlaHeamv( 'column-major', 'upper', N, 1.0, A, N, x, 1, 0.0, y, 1 );
console.log( y ); // eslint-disable-line no-console
