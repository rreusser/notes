/* eslint-disable camelcase, stdlib/require-file-extensions */

'use strict';

var discreteUniform = require( '@stdlib/random/array/discrete-uniform' );
var dla_syamv = require( '@stdlib/lapack/base/dla_syamv' );

var opts = {
	'dtype': 'float64'
};

var N = 3;
var A = discreteUniform( N * N, -10, 10, opts );
var x = discreteUniform( N, -10, 10, opts );
var y = discreteUniform( N, -10, 10, opts );

dla_syamv( 'row-major', 'upper', N, 1.0, A, N, x, 1, 0.0, y, 1 );
console.log( y ); // eslint-disable-line no-console
