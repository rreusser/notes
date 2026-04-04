

// TypeScript declarations for @stdlib/lapack/base/dlantp

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Returns the norm of a real triangular matrix in packed storage.
	*/
	(
		norm: string,
		uplo: string,
		diag: string,
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
* Returns the norm of a real triangular matrix in packed storage.
*/
declare var dlantp: Routine;

export = dlantp;
