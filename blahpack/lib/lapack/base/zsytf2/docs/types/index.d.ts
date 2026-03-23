

// TypeScript declarations for @stdlib/lapack/base/zsytf2

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Compute complex symmetric indefinite factorization with Bunch-Kaufman pivoting (unblocked)
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
* Compute complex symmetric indefinite factorization with Bunch-Kaufman pivoting (unblocked)
*/
declare var zsytf2: Routine;

export = zsytf2;
