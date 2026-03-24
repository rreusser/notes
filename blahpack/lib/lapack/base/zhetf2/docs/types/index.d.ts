

// TypeScript declarations for @stdlib/lapack/base/zhetf2

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Complex Hermitian indefinite factorization (unblocked Bunch-Kaufman)
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
		offsetIPIV: number
	): Float64Array;
}

/**
* Complex Hermitian indefinite factorization (unblocked Bunch-Kaufman)
*/
declare var zhetf2: Routine;

export = zhetf2;
