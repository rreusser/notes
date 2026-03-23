

// TypeScript declarations for @stdlib/lapack/base/zpotf2

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Compute Cholesky factorization of a Hermitian positive definite matrix (unblocked)
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
* Compute Cholesky factorization of a Hermitian positive definite matrix (unblocked)
*/
declare var zpotf2: Routine;

export = zpotf2;
