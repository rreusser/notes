

// TypeScript declarations for @stdlib/lapack/base/zhegs2

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Reduces a Hermitian-definite generalized eigenproblem to standard form (unblocked)
	*/
	(
		itype: number,
		uplo: string,
		N: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number,
		B: Float64Array,
		strideB1: number,
		strideB2: number,
		offsetB: number
	): Float64Array;
}

/**
* Reduces a Hermitian-definite generalized eigenproblem to standard form (unblocked)
*/
declare var zhegs2: Routine;

export = zhegs2;
