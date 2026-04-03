

// TypeScript declarations for @stdlib/lapack/base/zlapll

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Measures the linear dependence of two vectors.
	*/
	(
		N: number,
		x: Float64Array,
		strideX: number,
		offsetX: number,
		y: Float64Array,
		strideY: number,
		offsetY: number,
		ssmin: number
	): Float64Array;
}

/**
* Measures the linear dependence of two vectors.
*/
declare var zlapll: Routine;

export = zlapll;
