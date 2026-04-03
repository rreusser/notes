

// TypeScript declarations for @stdlib/lapack/base/zpptrs

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Solves a system of linear equations with a complex Hermitian positive definite matrix in packed storage using the Cholesky factorization.
	*/
	(
		uplo: string,
		N: number,
		nrhs: number,
		AP: Float64Array,
		strideAP: number,
		offsetAP: number,
		B: Float64Array,
		strideB1: number,
		strideB2: number,
		offsetB: number
	): Float64Array;
}

/**
* Solves a system of linear equations with a complex Hermitian positive definite matrix in packed storage using the Cholesky factorization.
*/
declare var zpptrs: Routine;

export = zpptrs;
