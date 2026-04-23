

'use strict';

var discreteUniform = require( '@stdlib/random/array/discrete-uniform' );
var ztplqt2 = require( '@stdlib/lapack/base/ztplqt2' );

var opts = {
	'dtype': 'float64'
};

var M = 3;
var N = 3;
var A = discreteUniform( M * N, -10, 10, opts );
var B = discreteUniform( M * N, -10, 10, opts );
var C = discreteUniform( M * N, -10, 10, opts );

// TODO: Adjust call to match the specific routine signature
ztplqt2( 'row-major', M, N, 1.0, A, N, B, N, 0.0, C, N );
console.log( C ); // eslint-disable-line no-console
