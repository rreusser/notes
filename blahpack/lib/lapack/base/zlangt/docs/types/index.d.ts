

// TypeScript declarations for @stdlib/lapack/base/zlangt

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Returns the norm of a complex tridiagonal matrix.
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
* Returns the norm of a complex tridiagonal matrix.
*/
declare var zlangt: Routine;

export = zlangt;
