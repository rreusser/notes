

// TypeScript declarations for @stdlib/lapack/base/dgbtrs

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Solve a banded system using the LU factorization from dgbtrf
	*/
	(
		trans: string,
		N: number,
		kl: number,
		ku: number,
		nrhs: number,
		AB: Float64Array,
		strideAB1: number,
		strideAB2: number,
		offsetAB: number,
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
* Solve a banded system using the LU factorization from dgbtrf
*/
declare var dgbtrs: Routine;

export = dgbtrs;
