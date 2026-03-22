

// TypeScript declarations for @stdlib/lapack/base/iladlc

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Find the last non-zero column of a real matrix.
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
* Find the last non-zero column of a real matrix.
*/
declare var iladlc: Routine;

export = iladlc;
