

// TypeScript declarations for @stdlib/blas/base/zhbmv

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Perform the Hermitian banded matrix-vector operation y := alpha*A*x + beta*y.
	*/
	(
		uplo: string,
		N: number,
		K: number,
		alpha: any,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number,
		x: Float64Array,
		strideX: number,
		offsetX: number,
		beta: any,
		y: Float64Array,
		strideY: number,
		offsetY: number
	): Float64Array;
}

/**
* Perform the Hermitian banded matrix-vector operation y := alpha*A*x + beta*y.
*/
declare var zhbmv: Routine;

export = zhbmv;
