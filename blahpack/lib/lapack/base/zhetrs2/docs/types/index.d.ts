

// TypeScript declarations for @stdlib/lapack/base/zhetrs2

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Complex Hermitian indefinite solve using factorization from ZHETRF
	*/
	(
		uplo: string,
		N: number,
		nrhs: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number,
		IPIV: Int32Array,
		strideIPIV: number,
		offsetIPIV: number,
		B: Float64Array,
		strideB1: number,
		strideB2: number,
		offsetB: number,
		WORK: Float64Array,
		strideWORK: number,
		offsetWORK: number
	): Float64Array;
}

/**
* Complex Hermitian indefinite solve using factorization from ZHETRF
*/
declare var zhetrs2: Routine;

export = zhetrs2;
