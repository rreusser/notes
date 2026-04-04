

// TypeScript declarations for @stdlib/lapack/base/zlacon

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Estimates the 1-norm of a square complex matrix using reverse communication.
	*/
	(
		N: number,
		v: Float64Array,
		strideV: number,
		offsetV: number,
		x: Float64Array,
		strideX: number,
		offsetX: number,
		est: number,
		kase: number
	): Float64Array;
}

/**
* Estimates the 1-norm of a square complex matrix using reverse communication.
*/
declare var zlacon: Routine;

export = zlacon;
