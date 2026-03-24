

// TypeScript declarations for @stdlib/lapack/base/zpttrf

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Computes the LDL^H factorization of a complex Hermitian positive definite tridiagonal matrix
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
* Computes the LDL^H factorization of a complex Hermitian positive definite tridiagonal matrix
*/
declare var zpttrf: Routine;

export = zpttrf;
