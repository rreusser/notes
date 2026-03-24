

// TypeScript declarations for @stdlib/blas/base/dswap

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Interchange two vectors
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
* Interchange two vectors
*/
declare var dswap: Routine;

export = dswap;
