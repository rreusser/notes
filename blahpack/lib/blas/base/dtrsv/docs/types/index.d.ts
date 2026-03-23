

// TypeScript declarations for @stdlib/blas/base/dtrsv

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Solve a triangular system of equations with a single right-hand side
	*/
	(
		uplo: string,
		trans: string,
		diag: string,
		N: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number,
		x: Float64Array,
		strideX: number,
		offsetX: number
	): Float64Array;
}

/**
* Solve a triangular system of equations with a single right-hand side
*/
declare var dtrsv: Routine;

export = dtrsv;
