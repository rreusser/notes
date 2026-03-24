

// TypeScript declarations for @stdlib/lapack/base/dptts2

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Solves a tridiagonal system using the LDL^T factorization from dpttrf
	*/
	(
		N: number,
		nrhs: number,
		d: Float64Array,
		strideD: number,
		offsetD: number,
		e: Float64Array,
		strideE: number,
		offsetE: number,
		B: Float64Array,
		strideB1: number,
		strideB2: number,
		offsetB: number
	): Float64Array;
}

/**
* Solves a tridiagonal system using the LDL^T factorization from dpttrf
*/
declare var dptts2: Routine;

export = dptts2;
