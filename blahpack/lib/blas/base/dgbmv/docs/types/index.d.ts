

// TypeScript declarations for @stdlib/blas/base/dgbmv

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Perform matrix-vector operation with a general band matrix
	*/
	(
		trans: string,
		M: number,
		N: number,
		kl: number,
		ku: number,
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
* Perform matrix-vector operation with a general band matrix
*/
declare var dgbmv: Routine;

export = dgbmv;
