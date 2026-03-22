

// TypeScript declarations for @stdlib/blas/base/dswap

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Interchange two double-precision floating-point vectors
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
* Interchange two double-precision floating-point vectors
*/
declare var dswap: Routine;

export = dswap;
