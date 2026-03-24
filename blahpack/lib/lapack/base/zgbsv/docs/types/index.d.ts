

// TypeScript declarations for @stdlib/lapack/base/zgbsv

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Solves a complex banded system of linear equations A*X = B using LU factorization
	*/
	(
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
* Solves a complex banded system of linear equations A*X = B using LU factorization
*/
declare var zgbsv: Routine;

export = zgbsv;
