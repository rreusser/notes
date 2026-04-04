

// TypeScript declarations for @stdlib/lapack/base/dppcon

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Estimates the reciprocal condition number of a real symmetric positive definite matrix in packed storage.
	*/
	(
		uplo: string,
		N: number,
		AP: Float64Array,
		strideAP: number,
		offsetAP: number,
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
* Estimates the reciprocal condition number of a real symmetric positive definite matrix in packed storage.
*/
declare var dppcon: Routine;

export = dppcon;
