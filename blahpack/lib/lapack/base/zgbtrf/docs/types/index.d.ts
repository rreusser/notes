

// TypeScript declarations for @stdlib/lapack/base/zgbtrf

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Compute LU factorization of a complex banded matrix (blocked)
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
* Compute LU factorization of a complex banded matrix (blocked)
*/
declare var zgbtrf: Routine;

export = zgbtrf;
