

// TypeScript declarations for @stdlib/blas/base/zdrot

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Apply a plane rotation to complex vectors
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
		incy: number,
		c: number,
		s: number
	): Float64Array;
}

/**
* Apply a plane rotation to complex vectors
*/
declare var zdrot: Routine;

export = zdrot;
