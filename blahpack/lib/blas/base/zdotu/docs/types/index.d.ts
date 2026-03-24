

// TypeScript declarations for @stdlib/blas/base/zdotu

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Compute unconjugated dot product of two complex vectors
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
* Compute unconjugated dot product of two complex vectors
*/
declare var zdotu: Routine;

export = zdotu;
