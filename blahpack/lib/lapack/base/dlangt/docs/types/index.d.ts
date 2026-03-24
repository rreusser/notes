

// TypeScript declarations for @stdlib/lapack/base/dlangt

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Compute the norm of a general tridiagonal matrix
	*/
	(
		norm: string,
		N: number,
		DL: Float64Array,
		strideDL: number,
		offsetDL: number,
		d: Float64Array,
		strideD: number,
		offsetD: number,
		DU: Float64Array,
		strideDU: number,
		offsetDU: number
	): Float64Array;
}

/**
* Compute the norm of a general tridiagonal matrix
*/
declare var dlangt: Routine;

export = dlangt;
