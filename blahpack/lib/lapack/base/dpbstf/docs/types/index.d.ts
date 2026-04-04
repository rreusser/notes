

// TypeScript declarations for @stdlib/lapack/base/dpbstf

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Computes a split Cholesky factorization of a real symmetric positive definite band matrix.
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
* Computes a split Cholesky factorization of a real symmetric positive definite band matrix.
*/
declare var dpbstf: Routine;

export = dpbstf;
