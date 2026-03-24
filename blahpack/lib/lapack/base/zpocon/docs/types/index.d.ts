

// TypeScript declarations for @stdlib/lapack/base/zpocon

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Estimate the reciprocal condition number of a complex positive definite matrix
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
		RWORK: Float64Array,
		strideRWORK: number,
		offsetRWORK: number
	): Float64Array;
}

/**
* Estimate the reciprocal condition number of a complex positive definite matrix
*/
declare var zpocon: Routine;

export = zpocon;
