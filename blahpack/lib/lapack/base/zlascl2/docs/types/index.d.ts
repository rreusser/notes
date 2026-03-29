

// TypeScript declarations for @stdlib/lapack/base/zlascl2

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Perform diagonal scaling on a complex matrix.
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
* Perform diagonal scaling on a complex matrix.
*/
declare var zlascl2: Routine;

export = zlascl2;
