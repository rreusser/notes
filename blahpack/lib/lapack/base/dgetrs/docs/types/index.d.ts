

// TypeScript declarations for @stdlib/lapack/base/dgetrs

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Solve a system of linear equations using LU factorization from DGETRF.
	*/
	(
		trans: string,
		N: number,
		nrhs: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number,
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
* Solve a system of linear equations using LU factorization from DGETRF.
*/
declare var dgetrs: Routine;

export = dgetrs;
