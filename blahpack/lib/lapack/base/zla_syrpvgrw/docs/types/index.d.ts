

// TypeScript declarations for @stdlib/lapack/base/zla_syrpvgrw

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Compute the reciprocal pivot growth factor norm(A)/norm(U) for a complex symmetric indefinite matrix
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
* Compute the reciprocal pivot growth factor norm(A)/norm(U) for a complex symmetric indefinite matrix
*/
declare var zla_syrpvgrw: Routine;

export = zla_syrpvgrw;
