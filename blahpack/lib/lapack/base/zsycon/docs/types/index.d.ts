

// TypeScript declarations for @stdlib/lapack/base/zsycon

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Estimate the reciprocal of the condition number of a complex symmetric indefinite matrix
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
		offsetWORK: number
	): Float64Array;
}

/**
* Estimate the reciprocal of the condition number of a complex symmetric indefinite matrix
*/
declare var zsycon: Routine;

export = zsycon;
