

// TypeScript declarations for @stdlib/lapack/base/dlanst

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Compute the norm of a symmetric tridiagonal matrix
	*/
	(
		norm: string,
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
* Compute the norm of a symmetric tridiagonal matrix
*/
declare var dlanst: Routine;

export = dlanst;
