

// TypeScript declarations for @stdlib/lapack/base/zla_herpvgrw

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Compute the reciprocal pivot growth factor for a complex Hermitian indefinite matrix.
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
* Compute the reciprocal pivot growth factor for a complex Hermitian indefinite matrix.
*/
declare var zla_herpvgrw: Routine;

export = zla_herpvgrw;
