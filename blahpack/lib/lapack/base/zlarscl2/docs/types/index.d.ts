

// TypeScript declarations for @stdlib/lapack/base/zlarscl2

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Perform reciprocal diagonal scaling on a complex matrix.
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
* Perform reciprocal diagonal scaling on a complex matrix.
*/
declare var zlarscl2: Routine;

export = zlarscl2;
