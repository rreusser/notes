

// TypeScript declarations for @stdlib/lapack/base/dlag2

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Compute the eigenvalues of a 2-by-2 generalized eigenvalue problem.
	*/
	(
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number,
		B: Float64Array,
		strideB1: number,
		strideB2: number,
		offsetB: number,
		safmin: number,
		scale1: number,
		scale2: number,
		wr1: number,
		wr2: number,
		wi: number
	): Float64Array;
}

/**
* Compute the eigenvalues of a 2-by-2 generalized eigenvalue problem.
*/
declare var dlag2: Routine;

export = dlag2;
