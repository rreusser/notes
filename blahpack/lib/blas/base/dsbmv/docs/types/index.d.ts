

// TypeScript declarations for @stdlib/blas/base/dsbmv

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Perform matrix-vector operation with a symmetric band matrix
	*/
	(
		uplo: string,
		N: number,
		K: number,
		alpha: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number,
		x: Float64Array,
		strideX: number,
		offsetX: number,
		beta: number,
		y: Float64Array,
		strideY: number,
		offsetY: number
	): Float64Array;
}

/**
* Perform matrix-vector operation with a symmetric band matrix
*/
declare var dsbmv: Routine;

export = dsbmv;
