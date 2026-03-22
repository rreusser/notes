

// TypeScript declarations for @stdlib/lapack/base/dpotri

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Compute the inverse of a real symmetric positive definite matrix using its Cholesky factorization
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
* Compute the inverse of a real symmetric positive definite matrix using its Cholesky factorization
*/
declare var dpotri: Routine;

export = dpotri;
