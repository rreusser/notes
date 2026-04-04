

// TypeScript declarations for @stdlib/lapack/base/dlansp

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Returns the value of the one-norm, Frobenius norm, infinity-norm, or the largest absolute value of a real symmetric matrix in packed storage.
	*/
	(
		norm: string,
		uplo: string,
		N: number,
		AP: Float64Array,
		strideAP: number,
		offsetAP: number,
		WORK: Float64Array,
		strideWORK: number,
		offsetWORK: number
	): Float64Array;
}

/**
* Returns the value of the one-norm, Frobenius norm, infinity-norm, or the largest absolute value of a real symmetric matrix in packed storage.
*/
declare var dlansp: Routine;

export = dlansp;
