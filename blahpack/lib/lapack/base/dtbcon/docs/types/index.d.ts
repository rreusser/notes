

// TypeScript declarations for @stdlib/lapack/base/dtbcon

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Estimates the reciprocal condition number of a real triangular band matrix.
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
		IWORK: Int32Array,
		strideIWORK: number,
		offsetIWORK: number
	): Float64Array;
}

/**
* Estimates the reciprocal condition number of a real triangular band matrix.
*/
declare var dtbcon: Routine;

export = dtbcon;
