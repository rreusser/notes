

'use strict';

// MODULES //

var base = require( './base.js' );


// MAIN //

/**
* Back-transforms eigenvectors after balancing by zgebal
*
* @param {string} job - specifies the operation type
* @param {string} side - specifies the operation type
* @param {NonNegativeInteger} N - number of columns
* @param {integer} ilo - ilo
* @param {integer} ihi - ihi
* @param {Float64Array} SCALE - input array
* @param {integer} strideSCALE - stride length for `SCALE`
* @param {NonNegativeInteger} offsetSCALE - starting index for `SCALE`
* @param {NonNegativeInteger} M - number of rows
* @param {Float64Array} V - output matrix
* @param {integer} strideV1 - stride of the first dimension of `V`
* @param {integer} strideV2 - stride of the second dimension of `V`
* @param {NonNegativeInteger} offsetV - starting index for `V`
* @returns {integer} status code (0 = success)
*/
function zgebak( job, side, N, ilo, ihi, SCALE, strideSCALE, offsetSCALE, M, V, strideV1, strideV2, offsetV ) { // eslint-disable-line max-len, max-params
	return base( job, side, N, ilo, ihi, SCALE, strideSCALE, offsetSCALE, M, V, strideV1, strideV2, offsetV ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = zgebak;
