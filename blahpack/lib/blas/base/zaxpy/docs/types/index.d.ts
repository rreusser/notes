

// TypeScript declarations for @stdlib/blas/base/zaxpy

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Scale a complex vector and add to another complex vector
	*/
	(
		N: number,
		za: any,
		x: Float64Array,
		strideX: number,
		offsetX: number,
		incx: number,
		y: Float64Array,
		strideY: number,
		offsetY: number,
		incy: number
	): Float64Array;
}

/**
* Scale a complex vector and add to another complex vector
*/
declare var zaxpy: Routine;

export = zaxpy;
