

// TypeScript declarations for @stdlib/lapack/base/zpptrf

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Computes the Cholesky factorization of a complex Hermitian positive definite matrix in packed storage.
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
* Computes the Cholesky factorization of a complex Hermitian positive definite matrix in packed storage.
*/
declare var zpptrf: Routine;

export = zpptrf;
