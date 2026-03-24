

// TypeScript declarations for @stdlib/lapack/base/dsytrs2

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Solves a symmetric indefinite system using Bunch-Kaufman factorization with BLAS-3
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
* Solves a symmetric indefinite system using Bunch-Kaufman factorization with BLAS-3
*/
declare var dsytrs2: Routine;

export = dsytrs2;
