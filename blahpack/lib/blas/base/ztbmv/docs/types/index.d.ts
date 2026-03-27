

// TypeScript declarations for @stdlib/blas/base/ztbmv

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Perform one of the matrix-vector operations x := A*x, x := A**T*x, or x := A**H*x, where A is a triangular band matrix
	*/
	(
		uplo: string,
		trans: string,
		diag: string,
		N: number,
		K: number,
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
* Perform one of the matrix-vector operations x := A*x, x := A**T*x, or x := A**H*x, where A is a triangular band matrix
*/
declare var ztbmv: Routine;

export = ztbmv;
