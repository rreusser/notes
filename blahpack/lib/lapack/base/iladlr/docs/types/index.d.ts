

// TypeScript declarations for @stdlib/lapack/base/iladlr

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Find the last non-zero row of a real matrix.
	*/
	(
		M: number,
		N: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number
	): Float64Array;
}

/**
* Find the last non-zero row of a real matrix.
*/
declare var iladlr: Routine;

export = iladlr;
