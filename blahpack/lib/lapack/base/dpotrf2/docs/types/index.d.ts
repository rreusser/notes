

// TypeScript declarations for @stdlib/lapack/base/dpotrf2

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Compute the Cholesky factorization of a real symmetric positive definite matrix using the recursive algorithm.
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
* Compute the Cholesky factorization of a real symmetric positive definite matrix using the recursive algorithm.
*/
declare var dpotrf2: Routine;

export = dpotrf2;
