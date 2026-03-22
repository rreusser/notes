

// TypeScript declarations for @stdlib/lapack/base/dpotrs

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Solve a symmetric positive definite system using the Cholesky factorization computed by dpotrf.
	*/
	(
		uplo: string,
		N: number,
		nrhs: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number,
		B: Float64Array,
		strideB1: number,
		strideB2: number,
		offsetB: number
	): Float64Array;
}

/**
* Solve a symmetric positive definite system using the Cholesky factorization computed by dpotrf.
*/
declare var dpotrs: Routine;

export = dpotrs;
