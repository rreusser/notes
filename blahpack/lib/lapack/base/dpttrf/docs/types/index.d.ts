

// TypeScript declarations for @stdlib/lapack/base/dpttrf

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Computes the LDL^T factorization of a real symmetric positive definite tridiagonal matrix
	*/
	(
		N: number,
		d: Float64Array,
		strideD: number,
		offsetD: number,
		e: Float64Array,
		strideE: number,
		offsetE: number
	): Float64Array;
}

/**
* Computes the LDL^T factorization of a real symmetric positive definite tridiagonal matrix
*/
declare var dpttrf: Routine;

export = dpttrf;
