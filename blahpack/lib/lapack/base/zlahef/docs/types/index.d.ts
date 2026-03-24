

// TypeScript declarations for @stdlib/lapack/base/zlahef

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Complex Hermitian indefinite panel factorization (blocked Bunch-Kaufman)
	*/
	(
		uplo: string,
		N: number,
		nb: number,
		kb: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number,
		IPIV: Int32Array,
		strideIPIV: number,
		offsetIPIV: number,
		W: Float64Array,
		strideW1: number,
		strideW2: number,
		offsetW: number
	): Float64Array;
}

/**
* Complex Hermitian indefinite panel factorization (blocked Bunch-Kaufman)
*/
declare var zlahef: Routine;

export = zlahef;
