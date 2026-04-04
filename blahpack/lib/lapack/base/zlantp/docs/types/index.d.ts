
// TypeScript declarations for @stdlib/lapack/base/zlantp

import { Complex128Array } from '@stdlib/types/array';

/**
* Interface describing the main API.
*/
interface Routine {
	/**
	* Returns the norm of a complex triangular matrix in packed storage.
	*
	* @param norm - norm type: `'max'`, `'one-norm'`, `'inf-norm'`, or `'frobenius'`
	* @param uplo - `'upper'` or `'lower'`
	* @param diag - `'unit'` or `'non-unit'`
	* @param N - order of the matrix
	* @param AP - packed triangular matrix
	* @param WORK - workspace array
	* @returns norm value
	*/
	(
		norm: string,
		uplo: string,
		diag: string,
		N: number,
		AP: Complex128Array,
		WORK: Float64Array
	): number;

	/**
	* Returns the norm of a complex triangular matrix in packed storage using alternative indexing semantics.
	*
	* @param norm - norm type: `'max'`, `'one-norm'`, `'inf-norm'`, or `'frobenius'`
	* @param uplo - `'upper'` or `'lower'`
	* @param diag - `'unit'` or `'non-unit'`
	* @param N - order of the matrix
	* @param AP - packed triangular matrix
	* @param strideAP - stride for AP (in complex elements)
	* @param offsetAP - starting index for AP (in complex elements)
	* @param WORK - workspace array
	* @param strideWORK - stride for WORK
	* @param offsetWORK - starting index for WORK
	* @returns norm value
	*/
	ndarray(
		norm: string,
		uplo: string,
		diag: string,
		N: number,
		AP: Complex128Array,
		strideAP: number,
		offsetAP: number,
		WORK: Float64Array,
		strideWORK: number,
		offsetWORK: number
	): number;
}

/**
* Returns the norm of a complex triangular matrix in packed storage.
*/
declare var zlantp: Routine;

export = zlantp;
