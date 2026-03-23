

// TypeScript declarations for @stdlib/lapack/base/zpbtf2

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Compute Cholesky factorization of a Hermitian positive definite banded matrix (unblocked)
	*/
	(
		uplo: string,
		N: number,
		kd: number,
		AB: Float64Array,
		strideAB1: number,
		strideAB2: number,
		offsetAB: number
	): Float64Array;
}

/**
* Compute Cholesky factorization of a Hermitian positive definite banded matrix (unblocked)
*/
declare var zpbtf2: Routine;

export = zpbtf2;
