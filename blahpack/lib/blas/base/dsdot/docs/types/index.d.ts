

// TypeScript declarations for @stdlib/blas/base/dsdot

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Compute the dot product of two vectors with extended precision accumulation
	*/
	(
		N: number,
		x: Float32Array,
		strideX: number,
		offsetX: number,
		incx: number,
		y: Float32Array,
		strideY: number,
		offsetY: number,
		incy: number
	): Float32Array;
}

/**
* Compute the dot product of two vectors with extended precision accumulation
*/
declare var dsdot: Routine;

export = dsdot;
