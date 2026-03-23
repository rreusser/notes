

// TypeScript declarations for @stdlib/lapack/base/dgbtf2

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Compute LU factorization of a banded matrix (unblocked)
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
* Compute LU factorization of a banded matrix (unblocked)
*/
declare var dgbtf2: Routine;

export = dgbtf2;
