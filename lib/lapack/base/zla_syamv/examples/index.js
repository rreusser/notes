/* eslint-disable camelcase, stdlib/require-file-extensions */

'use strict';

var discreteUniform = require( '@stdlib/random/array/discrete-uniform' );
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var zla_syamv = require( '@stdlib/lapack/base/zla_syamv' );

var opts = {
	'dtype': 'float64'
};

var N = 3;
var A = new Complex128Array( discreteUniform( 2 * N * N, -10, 10, opts ) );
var x = new Complex128Array( discreteUniform( 2 * N, -10, 10, opts ) );
var y = new Float64Array( N );

zla_syamv( 'row-major', 'upper', N, 1.0, A, N, x, 1, 0.0, y, 1 );
console.log( y ); // eslint-disable-line no-console
