

// TypeScript declarations for @stdlib/blas/base/drotm

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Apply a modified Givens plane rotation
	*/
	(
		N: number,
		x: Float64Array,
		strideX: number,
		offsetX: number,
		y: Float64Array,
		strideY: number,
		offsetY: number,
		dparam: Float64Array,
		strideDPARAM: number,
		offsetDPARAM: number
	): Float64Array;
}

/**
* Apply a modified Givens plane rotation
*/
declare var drotm: Routine;

export = drotm;
