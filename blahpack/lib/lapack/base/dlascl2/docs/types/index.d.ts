

// TypeScript declarations for @stdlib/lapack/base/dlascl2

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Perform diagonal scaling on a matrix.
	*/
	(
		M: number,
		N: number,
		d: Float64Array,
		strideD: number,
		offsetD: number,
		X: Float64Array,
		strideX1: number,
		strideX2: number,
		offsetX: number
	): Float64Array;
}

/**
* Perform diagonal scaling on a matrix.
*/
declare var dlascl2: Routine;

export = dlascl2;
