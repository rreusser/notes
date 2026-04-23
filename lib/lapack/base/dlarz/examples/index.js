'use strict';

var discreteUniform = require( '@stdlib/random/array/discrete-uniform' );
var Float64Array = require( '@stdlib/array/float64' );
var dlarz = require( './../lib' );

var opts = {
	'dtype': 'float64'
};

var M = 4;
var N = 4;
var L = 2;
var tau = 0.5;
var v = discreteUniform( L, -5, 5, opts );
var C = discreteUniform( M * N, -10, 10, opts );
var WORK = new Float64Array( N );

dlarz( 'column-major', 'left', M, N, L, v, 1, tau, C, M, WORK, 1 );
console.log( C ); // eslint-disable-line no-console
