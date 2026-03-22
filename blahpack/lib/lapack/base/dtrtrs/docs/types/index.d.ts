

// TypeScript declarations for @stdlib/lapack/base/dtrtrs

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Solve a triangular system of linear equations.
	*/
	(
		uplo: string,
		trans: string,
		diag: string,
		N: number,
		nrhs: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number,
		B: Float64Array,
		strideB1: number,
		strideB2: number,
		offsetB: number
	): Float64Array;
}

/**
* Solve a triangular system of linear equations.
*/
declare var dtrtrs: Routine;

export = dtrtrs;
