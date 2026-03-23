

// TypeScript declarations for @stdlib/lapack/base/zsytrs

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Solve a complex symmetric indefinite system using factorization from zsytrf
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
* Solve a complex symmetric indefinite system using factorization from zsytrf
*/
declare var zsytrs: Routine;

export = zsytrs;
