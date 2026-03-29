
'use strict';

// MODULES //

var format = require( '@stdlib/string/format' );
var base = require( './base.js' );


// MAIN //

/**
* CABS1: |re(z)| + |im(z)|.
*
* @param {Float64Array} v - Float64 view of complex array
* @param {integer} idx - index of real part
* @returns {number} CABS1 value
*/
function zgecon( norm, N, A, strideA1, strideA2, offsetA, anorm, rcond, WORK, strideWORK, offsetWORK, RWORK, strideRWORK, offsetRWORK ) { // eslint-disable-line max-len, max-params
	if ( norm !== 'max' && norm !== 'one-norm' && norm !== 'inf-norm' && norm !== 'frobenius' ) {
		throw new TypeError( format( 'invalid argument. First argument must be a valid norm value. Value: `%s`.', norm ) );
	}
	return base( norm, N, A, strideA1, strideA2, offsetA, anorm, rcond, WORK, strideWORK, offsetWORK, RWORK, strideRWORK, offsetRWORK ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = zgecon;
