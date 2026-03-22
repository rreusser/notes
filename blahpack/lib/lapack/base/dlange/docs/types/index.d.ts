

// TypeScript declarations for @stdlib/lapack/base/dlange

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Compute the value of the one norm, Frobenius norm, infinity norm, or largest absolute value of a matrix
	*/
	(
		norm: string,
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
* Compute the value of the one norm, Frobenius norm, infinity norm, or largest absolute value of a matrix
*/
declare var dlange: Routine;

export = dlange;
