
'use strict';

// MODULES //

var isOperationSide = require( '@stdlib/blas/base/assert/is-operation-side' );
var isMatrixTriangle = require( '@stdlib/blas/base/assert/is-matrix-triangle' );
var isTransposeOperation = require( '@stdlib/blas/base/assert/is-transpose-operation' );
var format = require( '@stdlib/string/format' );
var base = require( './base.js' );


// MAIN //

/**
* Overwrites a general matrix with a transformation from the orthogonal matrix Q returned by dsptrd.
*
* @param {string} side - specifies the operation type
* @param {string} uplo - specifies the operation type
* @param {string} trans - specifies the operation type
* @param {NonNegativeInteger} M - number of rows
* @param {NonNegativeInteger} N - number of columns
* @param {Float64Array} AP - input array
* @param {integer} strideAP - stride length for `AP`
* @param {NonNegativeInteger} offsetAP - starting index for `AP`
* @param {Float64Array} TAU - input array
* @param {integer} strideTAU - stride length for `TAU`
* @param {NonNegativeInteger} offsetTAU - starting index for `TAU`
* @param {Float64Array} C - input matrix
* @param {integer} strideC1 - stride of the first dimension of `C`
* @param {integer} strideC2 - stride of the second dimension of `C`
* @param {NonNegativeInteger} offsetC - starting index for `C`
* @param {Float64Array} WORK - output array
* @param {integer} strideWORK - stride length for `WORK`
* @param {NonNegativeInteger} offsetWORK - starting index for `WORK`
* @throws {TypeError} First argument must be a valid operation side
* @throws {TypeError} Second argument must be a valid matrix triangle
* @throws {TypeError} Third argument must be a valid transpose operation
* @returns {integer} status code (0 = success)
*/
function dopmtr( side, uplo, trans, M, N, AP, strideAP, offsetAP, TAU, strideTAU, offsetTAU, C, strideC1, strideC2, offsetC, WORK, strideWORK, offsetWORK ) { // eslint-disable-line max-len, max-params
	if ( !isOperationSide( side ) ) {
		throw new TypeError( format( 'invalid argument. First argument must be a valid operation side. Value: `%s`.', side ) );
	}
	if ( !isMatrixTriangle( uplo ) ) {
		throw new TypeError( format( 'invalid argument. Second argument must be a valid matrix triangle. Value: `%s`.', uplo ) );
	}
	if ( !isTransposeOperation( trans ) ) {
		throw new TypeError( format( 'invalid argument. Third argument must be a valid transpose operation. Value: `%s`.', trans ) );
	}
	return base( side, uplo, trans, M, N, AP, strideAP, offsetAP, TAU, strideTAU, offsetTAU, C, strideC1, strideC2, offsetC, WORK, strideWORK, offsetWORK ); // eslint-disable-line max-len
}


// EXPORTS //

module.exports = dopmtr;
