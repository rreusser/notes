

// TypeScript declarations for @stdlib/lapack/base/dsytrs

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Solve a symmetric indefinite system using the factorization from dsytrf
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
		offsetB: number
	): Float64Array;
}

/**
* Solve a symmetric indefinite system using the factorization from dsytrf
*/
declare var dsytrs: Routine;

export = dsytrs;
