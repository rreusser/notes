

// TypeScript declarations for @stdlib/lapack/base/dtrcon

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Estimates the reciprocal condition number of a triangular matrix
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
		IWORK: Int32Array,
		strideIWORK: number,
		offsetIWORK: number
	): Float64Array;
}

/**
* Estimates the reciprocal condition number of a triangular matrix
*/
declare var dtrcon: Routine;

export = dtrcon;
