

// TypeScript declarations for @stdlib/lapack/base/zpbcon

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Estimate reciprocal condition number of complex positive definite band matrix
	*/
	(
		uplo: string,
		N: number,
		kd: number,
		AB: Float64Array,
		strideAB1: number,
		strideAB2: number,
		offsetAB: number,
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
* Estimate reciprocal condition number of complex positive definite band matrix
*/
declare var zpbcon: Routine;

export = zpbcon;
