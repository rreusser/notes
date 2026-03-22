

// TypeScript declarations for @stdlib/lapack/base/dlauum

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Compute the product of an upper or lower triangular matrix with its transpose (blocked)
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
* Compute the product of an upper or lower triangular matrix with its transpose (blocked)
*/
declare var dlauum: Routine;

export = dlauum;
