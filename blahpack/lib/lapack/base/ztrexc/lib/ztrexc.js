

'use strict';

// MODULES //

var format = require( '@stdlib/string/format' );
var base = require( './base.js' );


// MAIN //

/**
* @license Apache-2.0
*
* @param {string} compq - compq
* @param {NonNegativeInteger} N - N
* @param {Complex128Array} T - T
* @param {PositiveInteger} LDT - leading dimension of `T`
* @param {Complex128Array} Q - Q
* @param {PositiveInteger} LDQ - leading dimension of `Q`
* @param {integer} ifst - ifst
* @param {integer} ilst - ilst
* @returns {*} result
*/
function ztrexc( compq, N, T, LDT, Q, LDQ, ifst, ilst ) { // eslint-disable-line max-len, max-params
	var sq1;
	var sq2;
	var st1;
	var st2;

	st1 = 1;
	st2 = LDT;
	sq1 = 1;
	sq2 = LDQ;
	if ( N < 0 ) {
		throw new RangeError( format( 'invalid argument. Second argument must be a nonnegative integer. Value: `%d`.', N ) );
	}
	return base( compq, N, T, st1, st2, 0, Q, sq1, sq2, 0, ifst, ilst );
}


// EXPORTS //

module.exports = ztrexc;
