

// TypeScript declarations for @stdlib/lapack/base/ztrcon

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Estimate the reciprocal condition number of a complex triangular matrix
	*/
	(
		norm: string,
		uplo: string,
		diag: string,
		N: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number,
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
* Estimate the reciprocal condition number of a complex triangular matrix
*/
declare var ztrcon: Routine;

export = ztrcon;
