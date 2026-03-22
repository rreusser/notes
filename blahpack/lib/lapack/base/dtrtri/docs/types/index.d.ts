

// TypeScript declarations for @stdlib/lapack/base/dtrtri

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Compute the inverse of a real upper or lower triangular matrix.
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
* Compute the inverse of a real upper or lower triangular matrix.
*/
declare var dtrtri: Routine;

export = dtrtri;
