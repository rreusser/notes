

// TypeScript declarations for @stdlib/lapack/base/zptcon

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Compute the reciprocal of the condition number of a complex Hermitian positive definite tridiagonal matrix
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
		RWORK: Float64Array,
		strideRWORK: number,
		offsetRWORK: number
	): Float64Array;
}

/**
* Compute the reciprocal of the condition number of a complex Hermitian positive definite tridiagonal matrix
*/
declare var zptcon: Routine;

export = zptcon;
