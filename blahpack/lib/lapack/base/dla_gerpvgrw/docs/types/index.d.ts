

// TypeScript declarations for @stdlib/lapack/base/dla_gerpvgrw

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Compute the reciprocal pivot growth factor norm(A)/norm(U).
	*/
	(
		N: number,
		ncols: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number,
		AF: Float64Array,
		strideAF1: number,
		strideAF2: number,
		offsetAF: number
	): Float64Array;
}

/**
* Compute the reciprocal pivot growth factor norm(A)/norm(U).
*/
declare var dla_gerpvgrw: Routine;

export = dla_gerpvgrw;
