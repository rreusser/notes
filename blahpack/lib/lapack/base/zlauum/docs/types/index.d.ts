

// TypeScript declarations for @stdlib/lapack/base/zlauum

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Compute the product of a complex triangular matrix with its conjugate transpose (blocked)
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
* Compute the product of a complex triangular matrix with its conjugate transpose (blocked)
*/
declare var zlauum: Routine;

export = zlauum;
