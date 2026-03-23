

// TypeScript declarations for @stdlib/lapack/base/dgbtrf

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Compute LU factorization of a banded matrix (blocked)
	*/
	(
		M: number,
		N: number,
		kl: number,
		ku: number,
		AB: Float64Array,
		strideAB1: number,
		strideAB2: number,
		offsetAB: number,
		IPIV: Int32Array,
		strideIPIV: number,
		offsetIPIV: number
	): Float64Array;
}

/**
* Compute LU factorization of a banded matrix (blocked)
*/
declare var dgbtrf: Routine;

export = dgbtrf;
