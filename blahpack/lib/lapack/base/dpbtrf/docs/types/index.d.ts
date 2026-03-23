

// TypeScript declarations for @stdlib/lapack/base/dpbtrf

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Compute Cholesky factorization of a symmetric positive definite banded matrix (blocked)
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
* Compute Cholesky factorization of a symmetric positive definite banded matrix (blocked)
*/
declare var dpbtrf: Routine;

export = dpbtrf;
