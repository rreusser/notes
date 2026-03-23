

// TypeScript declarations for @stdlib/lapack/base/dpbtf2

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Compute Cholesky factorization of a symmetric positive definite banded matrix (unblocked)
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
* Compute Cholesky factorization of a symmetric positive definite banded matrix (unblocked)
*/
declare var dpbtf2: Routine;

export = dpbtf2;
