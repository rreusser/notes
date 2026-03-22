

// TypeScript declarations for @stdlib/blas/base/dsymv

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Perform symmetric matrix-vector multiplication
	*/
	(
		uplo: string,
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
* Perform symmetric matrix-vector multiplication
*/
declare var dsymv: Routine;

export = dsymv;
