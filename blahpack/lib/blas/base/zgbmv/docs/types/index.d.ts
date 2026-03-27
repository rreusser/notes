

// TypeScript declarations for @stdlib/blas/base/zgbmv

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Perform one of the matrix-vector operations y := alpha*A*x + beta*y or y := alpha*A**T*x + beta*y or y := alpha*A**H*x + beta*y where A is a general band matrix.
	*/
	(
		trans: string,
		M: number,
		N: number,
		kl: number,
		ku: number,
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
* Perform one of the matrix-vector operations y := alpha*A*x + beta*y or y := alpha*A**T*x + beta*y or y := alpha*A**H*x + beta*y where A is a general band matrix.
*/
declare var zgbmv: Routine;

export = zgbmv;
