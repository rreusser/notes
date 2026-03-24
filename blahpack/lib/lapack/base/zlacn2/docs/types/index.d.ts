

// TypeScript declarations for @stdlib/lapack/base/zlacn2

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Estimate 1-norm of a square matrix using reverse communication
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
		kase: number,
		isave: Int32Array,
		strideISAVE: number,
		offsetISAVE: number
	): Float64Array;
}

/**
* Estimate 1-norm of a square matrix using reverse communication
*/
declare var zlacn2: Routine;

export = zlacn2;
