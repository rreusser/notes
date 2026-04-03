

// TypeScript declarations for @stdlib/lapack/base/zhpcon

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Estimates the reciprocal of the condition number of a complex Hermitian matrix in packed storage.
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
		offsetWORK: number
	): Float64Array;
}

/**
* Estimates the reciprocal of the condition number of a complex Hermitian matrix in packed storage.
*/
declare var zhpcon: Routine;

export = zhpcon;
