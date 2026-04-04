

// TypeScript declarations for @stdlib/lapack/base/ztpcon

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Estimates the reciprocal condition number of a complex triangular matrix in packed storage.
	*/
	(
		norm: string,
		uplo: string,
		diag: string,
		N: number,
		AP: Float64Array,
		strideAP: number,
		offsetAP: number,
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
* Estimates the reciprocal condition number of a complex triangular matrix in packed storage.
*/
declare var ztpcon: Routine;

export = ztpcon;
