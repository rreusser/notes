

// TypeScript declarations for @stdlib/lapack/base/dspcon

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Estimates the reciprocal of the condition number of a real symmetric matrix in packed storage.
	*/
	(
		uplo: string,
		N: number,
		AP: Float64Array,
		strideAP: number,
		offsetAP: number,
		IPIV: Int32Array,
		strideIPIV: number,
		offsetIPIV: number,
		anorm: number,
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
* Estimates the reciprocal of the condition number of a real symmetric matrix in packed storage.
*/
declare var dspcon: Routine;

export = dspcon;
