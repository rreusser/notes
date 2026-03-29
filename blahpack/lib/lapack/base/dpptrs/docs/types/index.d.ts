

// TypeScript declarations for @stdlib/lapack/base/dpptrs

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Solve a system of linear equations with a symmetric positive definite packed Cholesky-factored matrix
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
* Solve a system of linear equations with a symmetric positive definite packed Cholesky-factored matrix
*/
declare var dpptrs: Routine;

export = dpptrs;
