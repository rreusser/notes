

// TypeScript declarations for @stdlib/lapack/base/dla_syrpvgrw

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Compute the reciprocal pivot growth factor norm(A)/norm(U) for a symmetric indefinite matrix
	*/
	(
		uplo: number,
		N: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number,
		AF: Float64Array,
		strideAF1: number,
		strideAF2: number,
		offsetAF: number,
		IPIV: Int32Array,
		strideIPIV: number,
		offsetIPIV: number,
		WORK: Float64Array,
		strideWORK: number,
		offsetWORK: number
	): Float64Array;
}

/**
* Compute the reciprocal pivot growth factor norm(A)/norm(U) for a symmetric indefinite matrix
*/
declare var dla_syrpvgrw: Routine;

export = dla_syrpvgrw;
