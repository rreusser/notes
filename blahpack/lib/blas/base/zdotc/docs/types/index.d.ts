

// TypeScript declarations for @stdlib/blas/base/zdotc

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Compute the conjugate dot product of two complex vectors
	*/
	(
		N: number,
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
* Compute the conjugate dot product of two complex vectors
*/
declare var zdotc: Routine;

export = zdotc;
