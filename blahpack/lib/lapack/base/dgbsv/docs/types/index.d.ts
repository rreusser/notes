

// TypeScript declarations for @stdlib/lapack/base/dgbsv

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Solve a banded system of linear equations
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
* Solve a banded system of linear equations
*/
declare var dgbsv: Routine;

export = dgbsv;
