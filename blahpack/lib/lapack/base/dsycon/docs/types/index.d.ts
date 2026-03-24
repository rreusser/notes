

// TypeScript declarations for @stdlib/lapack/base/dsycon

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Estimates the reciprocal condition number of a symmetric indefinite matrix
	*/
	(
		uplo: string,
		N: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number,
		IPIV: Int32Array,
		strideIPIV: number,
		offsetIPIV: number,
		anorm: number,
		rcond: number,
		WORK: Float64Array,
		strideWORK: number,
		offsetWORK: number,
		IWORK: Int32Array,
		strideIWORK: number,
		offsetIWORK: number
	): Float64Array;
}

/**
* Estimates the reciprocal condition number of a symmetric indefinite matrix
*/
declare var dsycon: Routine;

export = dsycon;
