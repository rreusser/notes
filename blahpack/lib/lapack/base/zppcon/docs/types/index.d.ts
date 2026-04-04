

// TypeScript declarations for @stdlib/lapack/base/zppcon

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Estimates the reciprocal condition number of a complex Hermitian positive definite matrix in packed storage.
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
		RWORK: Float64Array,
		strideRWORK: number,
		offsetRWORK: number
	): Float64Array;
}

/**
* Estimates the reciprocal condition number of a complex Hermitian positive definite matrix in packed storage.
*/
declare var zppcon: Routine;

export = zppcon;
