

// TypeScript declarations for @stdlib/blas/base/dtrmv

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Perform one of the matrix-vector operations x := A*x or x := A**T*x where A is an N by N upper or lower triangular matrix.
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
* Perform one of the matrix-vector operations x := A*x or x := A**T*x where A is an N by N upper or lower triangular matrix.
*/
declare var dtrmv: Routine;

export = dtrmv;
