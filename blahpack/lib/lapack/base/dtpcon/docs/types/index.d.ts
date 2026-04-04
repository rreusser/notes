

// TypeScript declarations for @stdlib/lapack/base/dtpcon

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Estimates the reciprocal condition number of a real triangular matrix in packed storage.
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
		IWORK: Int32Array,
		strideIWORK: number,
		offsetIWORK: number
	): Float64Array;
}

/**
* Estimates the reciprocal condition number of a real triangular matrix in packed storage.
*/
declare var dtpcon: Routine;

export = dtpcon;
