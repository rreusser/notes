

// TypeScript declarations for @stdlib/blas/base/dtpmv

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Perform matrix-vector operation with a triangular packed matrix
	*/
	(
		uplo: string,
		trans: string,
		diag: string,
		N: number,
		AP: Float64Array,
		strideAP: number,
		offsetAP: number,
		x: Float64Array,
		strideX: number,
		offsetX: number
	): Float64Array;
}

/**
* Perform matrix-vector operation with a triangular packed matrix
*/
declare var dtpmv: Routine;

export = dtpmv;
