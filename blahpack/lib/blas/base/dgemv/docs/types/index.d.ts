

// TypeScript declarations for @stdlib/blas/base/dgemv

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Perform one of the matrix-vector operations y := alpha*A*x + beta*y or y := alpha*A**T*x + beta*y.
	*/
	(
		trans: string,
		M: number,
		N: number,
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
* Perform one of the matrix-vector operations y := alpha*A*x + beta*y or y := alpha*A**T*x + beta*y.
*/
declare var dgemv: Routine;

export = dgemv;
