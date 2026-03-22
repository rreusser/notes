

// TypeScript declarations for @stdlib/lapack/base/dpotrf

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Compute the Cholesky factorization of a real symmetric positive definite matrix.
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
* Compute the Cholesky factorization of a real symmetric positive definite matrix.
*/
declare var dpotrf: Routine;

export = dpotrf;
