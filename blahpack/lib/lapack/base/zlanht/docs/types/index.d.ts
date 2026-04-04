

// TypeScript declarations for @stdlib/lapack/base/zlanht

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Returns the norm of a complex Hermitian tridiagonal matrix.
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
* Returns the norm of a complex Hermitian tridiagonal matrix.
*/
declare var zlanht: Routine;

export = zlanht;
