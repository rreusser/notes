

// TypeScript declarations for @stdlib/lapack/base/dla_porpvgrw

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Compute the reciprocal pivot growth factor norm(A)/norm(U) for a symmetric positive-definite matrix.
	*/
	(
		uplo: number,
		ncols: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number,
		AF: Float64Array,
		strideAF1: number,
		strideAF2: number,
		offsetAF: number,
		WORK: Float64Array,
		strideWORK: number,
		offsetWORK: number
	): Float64Array;
}

/**
* Compute the reciprocal pivot growth factor norm(A)/norm(U) for a symmetric positive-definite matrix.
*/
declare var dla_porpvgrw: Routine;

export = dla_porpvgrw;
