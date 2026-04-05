

'use strict';

// MODULES //

var stride2offset = require( '@stdlib/strided/base/stride2offset' );
var format = require( '@stdlib/string/format' );
var isOperationSide = require( '@stdlib/blas/base/assert/is-operation-side' );
var max = require( '@stdlib/math/base/special/fast/max' );
var base = require( './base.js' );


// MAIN //

/**
* Back-transforms eigenvectors after balancing by dgebal.
*
* @param {string} job - job
* @param {string} side - side
* @param {NonNegativeInteger} N - N
* @param {integer} ilo - ilo
* @param {integer} ihi - ihi
* @param {Float64Array} SCALE - SCALE
* @param {integer} strideSCALE - strideSCALE
* @param {NonNegativeInteger} M - M
* @param {Float64Array} V - V
* @param {PositiveInteger} LDV - leading dimension of `V`
* @returns {*} result
*/
function dgebak( job, side, N, ilo, ihi, SCALE, strideSCALE, M, V, LDV ) { // eslint-disable-line max-len, max-params
	var oscale;
	var sv1;
	var sv2;

	sv1 = 1;
	sv2 = LDV;
	oscale = stride2offset( N, strideSCALE );
	if ( !isOperationSide( side ) ) {
		throw new TypeError( format( 'invalid argument. Second argument must be a valid operation side. Value: `%s`.', side ) );
	}
	if ( N < 0 ) {
		throw new RangeError( format( 'invalid argument. Third argument must be a nonnegative integer. Value: `%d`.', N ) );
	}
	if ( M < 0 ) {
		throw new RangeError( format( 'invalid argument. Eighth argument must be a nonnegative integer. Value: `%d`.', M ) );
	}
	if ( LDV < max( 1, M ) ) {
		throw new RangeError( format( 'invalid argument. Tenth argument must be greater than or equal to max(1,M). Value: `%d`.', LDV ) );
	}
	return base( job, side, N, ilo, ihi, SCALE, strideSCALE, oscale, M, V, sv1, sv2, 0 );
}


// EXPORTS //

module.exports = dgebak;
