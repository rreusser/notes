

// TypeScript declarations for @stdlib/lapack/base/zhecon

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Estimate the reciprocal condition number of a Hermitian indefinite matrix
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
* Estimate the reciprocal condition number of a Hermitian indefinite matrix
*/
declare var zhecon: Routine;

export = zhecon;
