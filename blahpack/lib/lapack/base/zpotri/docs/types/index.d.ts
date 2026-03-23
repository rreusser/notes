

// TypeScript declarations for @stdlib/lapack/base/zpotri

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Compute the inverse of a complex Hermitian positive definite matrix using its Cholesky factorization
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
* Compute the inverse of a complex Hermitian positive definite matrix using its Cholesky factorization
*/
declare var zpotri: Routine;

export = zpotri;
