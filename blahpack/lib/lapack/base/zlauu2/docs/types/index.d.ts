

// TypeScript declarations for @stdlib/lapack/base/zlauu2

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Compute the product of a complex triangular matrix with its conjugate transpose (unblocked)
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
* Compute the product of a complex triangular matrix with its conjugate transpose (unblocked)
*/
declare var zlauu2: Routine;

export = zlauu2;
