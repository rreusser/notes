

// TypeScript declarations for @stdlib/lapack/base/dpptrf

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Compute the Cholesky factorization of a real symmetric positive definite matrix stored in packed format.
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
* Compute the Cholesky factorization of a real symmetric positive definite matrix stored in packed format.
*/
declare var dpptrf: Routine;

export = dpptrf;
