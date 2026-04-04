

// TypeScript declarations for @stdlib/lapack/base/zpftrf

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Computes the Cholesky factorization of a complex Hermitian positive definite matrix in Rectangular Full Packed format.
	*/
	(
		transr: string,
		uplo: string,
		N: number,
		a: Float64Array,
		stride: number,
		offset: number
	): Float64Array;
}

/**
* Computes the Cholesky factorization of a complex Hermitian positive definite matrix in Rectangular Full Packed format.
*/
declare var zpftrf: Routine;

export = zpftrf;
