

// TypeScript declarations for @stdlib/lapack/base/dptcon

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Compute the reciprocal of the condition number of a real symmetric positive definite tridiagonal matrix
	*/
	(
		N: number,
		d: Float64Array,
		strideD: number,
		offsetD: number,
		e: Float64Array,
		strideE: number,
		offsetE: number,
		anorm: number,
		rcond: number,
		WORK: Float64Array,
		strideWORK: number,
		offsetWORK: number
	): Float64Array;
}

/**
* Compute the reciprocal of the condition number of a real symmetric positive definite tridiagonal matrix
*/
declare var dptcon: Routine;

export = dptcon;
