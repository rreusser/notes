

// TypeScript declarations for @stdlib/lapack/base/dlasq2

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Compute all eigenvalues of a symmetric positive definite tridiagonal matrix via dqds
	*/
	(
		N: number,
		z: Float64Array,
		stride: number,
		offset: number
	): Float64Array;
}

/**
* Compute all eigenvalues of a symmetric positive definite tridiagonal matrix via dqds
*/
declare var dlasq2: Routine;

export = dlasq2;
