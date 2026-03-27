

'use strict';

// MODULES //

var stride2offset = require( '@stdlib/strided/base/stride2offset' );
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
	return base( job, N, A, sa1, sa2, 0, SCALE, strideSCALE, oscale );
}


// EXPORTS //

module.exports = zgebal;
