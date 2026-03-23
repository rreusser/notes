

// TypeScript declarations for @stdlib/lapack/base/dpocon

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Estimate the reciprocal condition number of a symmetric positive definite matrix
	*/
	(
		uplo: string,
		N: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number,
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
* Estimate the reciprocal condition number of a symmetric positive definite matrix
*/
declare var dpocon: Routine;

export = dpocon;
