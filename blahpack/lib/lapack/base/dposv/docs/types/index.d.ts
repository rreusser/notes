

// TypeScript declarations for @stdlib/lapack/base/dposv

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Compute the solution to a real system of linear equations A*X=B where A is symmetric positive definite.
	*/
	(
		uplo: string,
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
* Compute the solution to a real system of linear equations A*X=B where A is symmetric positive definite.
*/
declare var dposv: Routine;

export = dposv;
