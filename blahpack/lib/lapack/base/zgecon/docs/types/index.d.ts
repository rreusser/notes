

// TypeScript declarations for @stdlib/lapack/base/zgecon

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Estimate the reciprocal condition number of a complex general matrix
	*/
	(
		norm: string,
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
* Estimate the reciprocal condition number of a complex general matrix
*/
declare var zgecon: Routine;

export = zgecon;
