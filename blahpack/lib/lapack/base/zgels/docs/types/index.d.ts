

// TypeScript declarations for @stdlib/lapack/base/zgels

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Solve complex linear least squares using QR or LQ factorization
	*/
	(
		trans: string,
		M: number,
		N: number,
		nrhs: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number,
		B: Float64Array,
		strideB1: number,
		strideB2: number,
		offsetB: number,
		WORK: Float64Array,
		strideWORK: number,
		offsetWORK: number,
		lwork: number
	): Float64Array;
}

/**
* Solve complex linear least squares using QR or LQ factorization
*/
declare var zgels: Routine;

export = zgels;
