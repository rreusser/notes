

'use strict';

var discreteUniform = require( '@stdlib/random/array/discrete-uniform' );
var dlarfgp = require( '@stdlib/lapack/base/dlarfgp' );

var opts = {
	'dtype': 'float64'
};

var N = 10;
var x = discreteUniform( N, -10, 10, opts );
var y = discreteUniform( N, -10, 10, opts );

// TODO: Adjust call to match the specific routine signature
dlarfgp( N, 1.0, x, 1, y, 1 );
console.log( y ); // eslint-disable-line no-console
