

// TypeScript declarations for @stdlib/lapack/base/dtrti2

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Compute the inverse of a real upper or lower triangular matrix (unblocked algorithm).
	*/
	(
		uplo: string,
		diag: string,
		N: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number
	): Float64Array;
}

/**
* Compute the inverse of a real upper or lower triangular matrix (unblocked algorithm).
*/
declare var dtrti2: Routine;

export = dtrti2;
