

// TypeScript declarations for @stdlib/lapack/base/dsterf

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Compute all eigenvalues of a symmetric tridiagonal matrix
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
* Compute all eigenvalues of a symmetric tridiagonal matrix
*/
declare var dsterf: Routine;

export = dsterf;
