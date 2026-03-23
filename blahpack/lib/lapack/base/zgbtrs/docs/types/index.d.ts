

// TypeScript declarations for @stdlib/lapack/base/zgbtrs

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Solve a complex banded system using LU factorization
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
* Solve a complex banded system using LU factorization
*/
declare var zgbtrs: Routine;

export = zgbtrs;
