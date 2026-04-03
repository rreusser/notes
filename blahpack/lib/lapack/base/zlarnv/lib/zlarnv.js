
'use strict';

// MODULES //

var stride2offset = require( '@stdlib/strided/base/stride2offset' );
var base = require( './base.js' );


// MAIN //

/**
* Returns a vector of complex random numbers from a uniform or normal distribution.
*
* @param {integer} idist - distribution type: 1=uniform(0,1), 2=uniform(-1,1), 3=normal(0,1), 4=uniform disc, 5=uniform circle
* @param {Int32Array} iseed - seed array of 4 integers
* @param {integer} strideISEED - stride for iseed
* @param {NonNegativeInteger} N - number of complex random numbers to generate
* @param {Complex128Array} x - output array
* @param {integer} stride - stride for x (in complex elements)
* @returns {*} result
*/
function zlarnv( idist, iseed, strideISEED, N, x, stride ) {
	var oiseed;
	var ox;

	oiseed = stride2offset( N, strideISEED );
	ox = stride2offset( N, stride );
	return base( idist, iseed, strideISEED, oiseed, N, x, stride, ox );
}


// EXPORTS //

module.exports = zlarnv;
