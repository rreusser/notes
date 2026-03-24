

// TypeScript declarations for @stdlib/lapack/base/dlantr

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Computes the norm of a real triangular matrix
	*/
	(
		norm: string,
		uplo: string,
		diag: string,
		M: number,
		N: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number,
		WORK: Float64Array,
		strideWORK: number,
		offsetWORK: number
	): Float64Array;
}

/**
* Computes the norm of a real triangular matrix
*/
declare var dlantr: Routine;

export = dlantr;
