

// TypeScript declarations for @stdlib/lapack/base/zhetrf

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Complex Hermitian indefinite factorization (blocked Bunch-Kaufman)
	*/
	(
		uplo: string,
		N: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number,
		IPIV: Int32Array,
		strideIPIV: number,
		offsetIPIV: number,
		WORK: Float64Array,
		strideWORK: number,
		offsetWORK: number,
		lwork: number
	): Float64Array;
}

/**
* Complex Hermitian indefinite factorization (blocked Bunch-Kaufman)
*/
declare var zhetrf: Routine;

export = zhetrf;
