

// TypeScript declarations for @stdlib/lapack/base/dlauu2

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Compute the product of an upper or lower triangular matrix with its transpose (unblocked)
	*/
	(
		uplo: string,
		N: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number
	): Float64Array;
}

/**
* Compute the product of an upper or lower triangular matrix with its transpose (unblocked)
*/
declare var dlauu2: Routine;

export = dlauu2;
