

// TypeScript declarations for @stdlib/blas/base/dcopy

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Copy a vector x to a vector y
	*/
	(
		N: number,
		x: Float64Array,
		strideX: number,
		offsetX: number,
		y: Float64Array,
		strideY: number,
		offsetY: number
	): Float64Array;
}

/**
* Copy a vector x to a vector y
*/
declare var dcopy: Routine;

export = dcopy;
