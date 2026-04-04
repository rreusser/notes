

// TypeScript declarations for @stdlib/lapack/base/dpptri

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Computes the inverse of a real symmetric positive definite matrix in packed storage using the Cholesky factorization.
	*/
	(
		uplo: string,
		N: number,
		AP: Float64Array,
		stride: number,
		offset: number
	): Float64Array;
}

/**
* Computes the inverse of a real symmetric positive definite matrix in packed storage using the Cholesky factorization.
*/
declare var dpptri: Routine;

export = dpptri;
