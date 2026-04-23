
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
* @param {string} job - job
* @param {NonNegativeInteger} N - N
* @param {Complex128Array} A - A
* @param {PositiveInteger} LDA - leading dimension of `A`
* @param {Float64Array} SCALE - SCALE
* @param {integer} strideSCALE - strideSCALE
* @returns {*} result
*/
function zgebal( job, N, A, LDA, SCALE, strideSCALE ) {
	var oscale;
	var sa1;
	var sa2;

	sa1 = 1;
	sa2 = LDA;
	oscale = stride2offset( N, strideSCALE );
	if ( N < 0 ) {
		throw new RangeError( format( 'invalid argument. Second argument must be a nonnegative integer. Value: `%d`.', N ) );
	}
	if ( LDA < max( 1, N ) ) {
		throw new RangeError( format( 'invalid argument. Fourth argument must be greater than or equal to max(1,N). Value: `%d`.', LDA ) );
	}
	if ( job !== 'none' && job !== 'permute' ) {
		throw new TypeError( format( 'invalid argument. First argument must be a valid `job` value. Value: `%s`.', job ) );
	}
	return base( job, N, A, sa1, sa2, 0, SCALE, strideSCALE, oscale );
}


// EXPORTS //

module.exports = zgebal;
