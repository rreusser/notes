

'use strict';

// MODULES //

var stride2offset = require( '@stdlib/strided/base/stride2offset' );
var base = require( './base.js' );


// MAIN //

/**
* Back-transforms eigenvectors after balancing by zgebal.
*
* @param {string} job - job
* @param {string} side - side
* @param {NonNegativeInteger} N - N
* @param {integer} ilo - ilo
* @param {integer} ihi - ihi
* @param {Float64Array} SCALE - SCALE
* @param {integer} strideSCALE - strideSCALE
* @param {NonNegativeInteger} M - M
* @param {Complex128Array} V - V
* @param {PositiveInteger} LDV - leading dimension of `V`
* @returns {*} result
*/
function zgebak( job, side, N, ilo, ihi, SCALE, strideSCALE, M, V, LDV ) { // eslint-disable-line max-len, max-params
	var oscale;
	var sv1;
	var sv2;

	sv1 = 1;
	sv2 = LDV;
	oscale = stride2offset( N, strideSCALE );
	return base( job, side, N, ilo, ihi, SCALE, strideSCALE, oscale, M, V, sv1, sv2, 0 );
}


// EXPORTS //

module.exports = zgebak;
