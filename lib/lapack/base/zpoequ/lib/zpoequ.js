

'use strict';

// MODULES //

var stride2offset = require( '@stdlib/strided/base/stride2offset' );
var format = require( '@stdlib/string/format' );
var max = require( '@stdlib/math/base/special/fast/max' );
var base = require( './base.js' );


// MAIN //

/**
* @license Apache-2.0
*
* @param {NonNegativeInteger} N - N
* @param {Complex128Array} A - A
* @param {PositiveInteger} LDA - leading dimension of `A`
* @param {Float64Array} s - s
* @param {integer} strideS - strideS
* @returns {*} result
*/
function zpoequ( N, A, LDA, s, strideS ) {
	var sa1;
	var sa2;
	var os;

	sa1 = 1;
	sa2 = LDA;
	os = stride2offset( N, strideS );
	if ( N < 0 ) {
		throw new RangeError( format( 'invalid argument. First argument must be a nonnegative integer. Value: `%d`.', N ) );
	}
	if ( LDA < max( 1, N ) ) {
		throw new RangeError( format( 'invalid argument. Third argument must be greater than or equal to max(1,N). Value: `%d`.', LDA ) );
	}
	return base( N, A, sa1, sa2, 0, s, strideS, os );
}


// EXPORTS //

module.exports = zpoequ;
