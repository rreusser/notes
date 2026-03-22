

// TypeScript declarations for @stdlib/blas/base/drot

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Apply a Givens plane rotation
	*/
	(
		N: number,
		x: Float64Array,
		strideX: number,
		offsetX: number,
		y: Float64Array,
		strideY: number,
		offsetY: number,
		c: number,
		s: number
	): Float64Array;
}

/**
* Apply a Givens plane rotation
*/
declare var drot: Routine;

export = drot;
