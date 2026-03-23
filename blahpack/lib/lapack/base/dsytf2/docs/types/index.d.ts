

// TypeScript declarations for @stdlib/lapack/base/dsytf2

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Compute symmetric indefinite factorization with Bunch-Kaufman pivoting (unblocked)
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
* Compute symmetric indefinite factorization with Bunch-Kaufman pivoting (unblocked)
*/
declare var dsytf2: Routine;

export = dsytf2;
