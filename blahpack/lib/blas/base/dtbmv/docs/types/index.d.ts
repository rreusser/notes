

// TypeScript declarations for @stdlib/blas/base/dtbmv

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Perform matrix-vector operation with a triangular band matrix
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
* Perform matrix-vector operation with a triangular band matrix
*/
declare var dtbmv: Routine;

export = dtbmv;
