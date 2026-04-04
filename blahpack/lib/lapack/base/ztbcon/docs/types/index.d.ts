

// TypeScript declarations for @stdlib/lapack/base/ztbcon

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Estimates the reciprocal condition number of a complex triangular band matrix.
	*/
	(
		norm: string,
		uplo: string,
		diag: string,
		N: number,
		kd: number,
		AB: Float64Array,
		strideAB1: number,
		strideAB2: number,
		offsetAB: number,
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
* Estimates the reciprocal condition number of a complex triangular band matrix.
*/
declare var ztbcon: Routine;

export = ztbcon;
