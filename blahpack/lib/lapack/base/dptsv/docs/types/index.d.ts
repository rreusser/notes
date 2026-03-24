

// TypeScript declarations for @stdlib/lapack/base/dptsv

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Solves a real symmetric positive definite tridiagonal system of linear equations
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
* Solves a real symmetric positive definite tridiagonal system of linear equations
*/
declare var dptsv: Routine;

export = dptsv;
