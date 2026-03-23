

// TypeScript declarations for @stdlib/lapack/base/dpbtrs

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Solve a banded symmetric positive definite system using Cholesky factorization
	*/
	(
		uplo: string,
		N: number,
		kd: number,
		nrhs: number,
		AB: Float64Array,
		strideAB1: number,
		strideAB2: number,
		offsetAB: number,
		B: Float64Array,
		strideB1: number,
		strideB2: number,
		offsetB: number
	): Float64Array;
}

/**
* Solve a banded symmetric positive definite system using Cholesky factorization
*/
declare var dpbtrs: Routine;

export = dpbtrs;
