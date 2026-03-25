

// TypeScript declarations for @stdlib/lapack/base/dlapll

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Measures linear dependence of two vectors via QR factorization and SVD
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
* Measures linear dependence of two vectors via QR factorization and SVD
*/
declare var dlapll: Routine;

export = dlapll;
