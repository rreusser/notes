

// TypeScript declarations for @stdlib/lapack/base/dsytrf

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Compute symmetric indefinite factorization with Bunch-Kaufman pivoting (blocked)
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
* Compute symmetric indefinite factorization with Bunch-Kaufman pivoting (blocked)
*/
declare var dsytrf: Routine;

export = dsytrf;
