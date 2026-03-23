

// TypeScript declarations for @stdlib/blas/base/ztrsv

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Solve a complex triangular system of equations with a single right-hand side
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
* Solve a complex triangular system of equations with a single right-hand side
*/
declare var ztrsv: Routine;

export = ztrsv;
